pub use crate::lexer::{TokenIndex, TokenLength};

use std::fmt;
use std::io;
use std::cmp;
use std::iter;
use std::rc::Rc;
use std::collections::{BinaryHeap, HashMap};

use crate::utils::delegate_fmt;
use crate::source::{ModuleSource, SourceText};
use crate::runtime::data::Chunk;


// metadata attached to parser output for error handling and debug output
// will probably be attached to the statement level


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DebugSymbol {
    pub start: TokenIndex,
    pub end: TokenIndex,
}

impl From<(TokenIndex, TokenIndex)> for DebugSymbol {
    fn from(tuple: (TokenIndex, TokenIndex)) -> Self {
        let (start, end) = tuple;
        DebugSymbol { start, end }
    }
}

// Container for debug symbols generated for bytecode
// Should contain a DebugSymbol for each opcode in the 
// associated Chunk, and in the same order.
#[derive(Clone)]
pub struct ChunkDebugSymbols {
    source: ModuleSource,
    symbols: Vec<(DebugSymbol, u8)>,  // run length encoding
}

impl ChunkDebugSymbols {
    pub fn new(source: ModuleSource) -> Self {
        ChunkDebugSymbols {
            source, symbols: Vec::new(),
        }
    }
    
    pub fn symbols(&self) -> impl Iterator<Item=&DebugSymbol> { 
        self.symbols.iter().flat_map(
            |(sym, count)| iter::repeat(sym).take(usize::from(*count))
        )
    }
    
    pub fn push(&mut self, symbol: DebugSymbol) {
        match self.symbols.last_mut() {
            Some((last, ref mut count)) if *last == symbol && *count < u8::MAX => { 
                *count += 1 
            },
            _ => { self.symbols.push((symbol, 1)) }
        }
    }
}

// Resolved Symbols

#[derive(Debug, Clone)]
pub struct ResolvedSymbol {
    lines: Vec<Rc<String>>,
    lineno: usize,  // line number at the start of the symbol
    start: usize,   // start,end indices into self.text
    end: usize,
}

impl ResolvedSymbol {
    pub fn new(lines: Vec<Rc<String>>, lineno: usize, start: usize, end: usize) -> Self {
        ResolvedSymbol { lines, lineno, start, end }
    }
    
    pub fn is_multiline(&self) -> bool { self.lines.len() > 1 }
    pub fn line_count(&self) -> usize { self.lines.len() }
    
    pub fn lineno(&self) -> usize { self.lineno }
    pub fn start(&self) -> usize { self.start }
    pub fn end(&self) -> usize { self.end }
    
    pub fn start_col(&self) -> usize { self.start }
    pub fn end_col(&self) -> usize {
        let offset = self.lines.iter()
            .take(self.lines.len() - 1)
            .map(|line| line.len())
            .reduce(|acc, n| acc + n)
            .unwrap_or(0);
        
        self.end - offset  // subtract index to start of final line
    }
    
    pub fn iter_whole_lines(&self) -> impl Iterator<Item=&str> { 
        self.lines.iter().map(|s| s.as_str())
    }
    
    // write just the symbol text itself
    pub fn iter_lines(&self) -> impl Iterator<Item=&str> { 
        self.lines.iter().scan(0, |cur_line_start, line| {
            let cur_line_end = *cur_line_start + line.len();
            
            let result;
            if (*cur_line_start <= self.start) && (self.start < cur_line_end) {
                let start = self.start - *cur_line_start;
                result = &line[start..];
            } else if (*cur_line_start <= self.end) && (self.end < cur_line_end) {
                let end = self.end - *cur_line_start;
                result = &line[..end];
            } else {
                result = line.as_str();
            };
            
            *cur_line_start = cur_line_end;
            Some(result)
        })
    }

}

impl fmt::Display for ResolvedSymbol {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        for line in self.iter_lines() {
            fmt.write_str(line.trim_end())?;
            fmt.write_str("\n")?;
        }
        Ok(())
    }
}


// Resolved Symbol Formatting

// Symbol Resolution

pub type ResolvedSymbolTable<'s> = HashMap<&'s DebugSymbol, Result<ResolvedSymbol, SymbolResolutionError>>;

pub trait DebugSymbolResolver {
    fn resolve_symbols<'s, S>(&self, symbols: S) -> io::Result<ResolvedSymbolTable<'s>> where S: Iterator<Item=&'s DebugSymbol>;
}

impl DebugSymbolResolver for ModuleSource {
    fn resolve_symbols<'s, S>(&self, symbols: S) -> io::Result<ResolvedSymbolTable<'s>> where S: Iterator<Item=&'s DebugSymbol> {
        match self.source_text()? {
            SourceText::String(string) => {
                Ok(resolve_debug_symbols(string.chars().map(Ok), symbols))
            },
            SourceText::File(char_iter) => {
                Ok(resolve_debug_symbols(char_iter, symbols))
            }
        }
    }
}

fn resolve_debug_symbols<'s>(source: impl Iterator<Item=io::Result<char>>, symbols: impl Iterator<Item=&'s DebugSymbol>) -> ResolvedSymbolTable<'s> {
    // put all the symbols into a priority queue based on first occurrence in the source text
    let mut next_symbols = BinaryHeap::new();
    next_symbols.extend(symbols.map(|sym| IndexSort(sym, SortIndex::Start)).map(cmp::Reverse));
    
    let mut open_symbols = BinaryHeap::new();
    let mut active_symbols = HashMap::<&DebugSymbol, (Vec<Rc<String>>, usize, usize)>::new(); // values are (buffer, line number, start index)
    let mut closing_symbols = HashMap::<&DebugSymbol, (Vec<Rc<String>>, usize, usize, usize)>::new();
    let mut resolved_symbols = ResolvedSymbolTable::new();
    
    let mut lineno = 1;  // count lines
    let mut current_line = String::new();
    for (char_result, index) in source.zip((0 as TokenIndex)..) {
        // check for io::Errors 
        let c = match char_result {
            Ok(c) => c,
            
            Err(..) => {
                // drop all open symbols and close all closing symbols
                active_symbols.clear();
                for cmp::Reverse(IndexSort(symbol,..)) in open_symbols.drain() {
                    resolved_symbols.insert(symbol, Err(ErrorKind::IOError.into()));
                }
                
                // we've already have the required text for all closing symbols, so we don't need to drop them
                // just add a suffix that indicates that there was some trailing line content that was lost due to an error
                let error_rc = Rc::new(current_line.clone() + "...???");
                for (symbol, (mut lines, lineno, start_index, end_index)) in closing_symbols.drain() {
                    lines.push(error_rc.clone());
                    let resolved = ResolvedSymbol::new(lines, lineno, start_index, end_index);
                    resolved_symbols.insert(symbol, Ok(resolved));
                }
                
                current_line.clear();
                current_line += "???...";
            
                break;
            },
        };
        
        // add the char to the current line
        current_line.push(c);
        
        // if we are at the start of a new symbol, open it
        while matches!(next_symbols.peek(), Some(&cmp::Reverse(IndexSort(ref sym,..))) if index == sym.start) {
            let symbol = next_symbols.pop().unwrap().0.0;
            if !active_symbols.contains_key(&symbol) {
                open_symbols.push(cmp::Reverse(IndexSort(symbol, SortIndex::End)));
                
                let start_index = current_line.len() - 1;
                active_symbols.insert(symbol, (Vec::new(), lineno, start_index));
            }
        }
        
        // if we are at the end of an open symbol, mark it as closing
        while matches!(open_symbols.peek(), Some(&cmp::Reverse(IndexSort(ref sym,..))) if index == sym.end) {
            let symbol = open_symbols.pop().unwrap().0.0;
            if let Some((lines, lineno, start_index)) = active_symbols.remove(&symbol) {
                if !closing_symbols.contains_key(&symbol) {
                    
                    let total_len = lines.iter()
                        .map(|line| line.len())
                        .reduce(|acc, n| acc+n)
                        .unwrap_or(0);
                    
                    let end_index = total_len + current_line.len() - 1;
                    closing_symbols.insert(symbol, (lines, lineno, start_index, end_index));
                }
            }
        }
        
        // once we complete the current line, add it to all open and closing symbols
        if c == '\n' {
            lineno +=1; // count newlines
            let line_rc = Rc::new(current_line.clone());
            
            // open symbols
            for (ref mut lines, ..) in active_symbols.values_mut() {
                lines.push(line_rc.clone());
            }
            
            // close all closing symbols
            for (symbol, (mut lines, lineno, start_index, end_index)) in closing_symbols.drain() {
                lines.push(line_rc.clone());
                
                let resolved = ResolvedSymbol::new(lines, lineno, start_index, end_index);
                resolved_symbols.insert(symbol, Ok(resolved));
            }
            
            // prepare buffer for next line
            current_line.clear();
        }
        
        // println!("{}: {}", lineno, current_line);
        // println!("next_symbols: {:?}", next_symbols);
        // println!("open_symbols: {:?}", open_symbols);
        // println!("active_symbols: {:?}", active_symbols);
        // println!("closing_symbols: {:?}", closing_symbols);
        // println!("resolved_symbols: {:?}", resolved_symbols);
        
        if next_symbols.is_empty() && active_symbols.is_empty() && closing_symbols.is_empty() {
            break;
        }
    }
    
    // process any active or closing symbols left after we hit EOF
    for cmp::Reverse(IndexSort(symbol,..)) in open_symbols.drain() {
        resolved_symbols.insert(symbol, Err(ErrorKind::EOFReached.into()));
    }
    
    let line_rc = Rc::new(current_line.clone());
    for (symbol, (mut lines, lineno, start_index, end_index)) in closing_symbols.drain() {
        lines.push(line_rc.clone());
        
        let resolved = ResolvedSymbol::new(lines, lineno, start_index, end_index);
        resolved_symbols.insert(symbol, Ok(resolved));
    }
    
    resolved_symbols
}


#[derive(Debug)]
enum SortIndex { Start, End }

// comparison based on start or end index
#[derive(Debug)]
struct IndexSort<'s>(&'s DebugSymbol, SortIndex);

impl IndexSort<'_> {
    fn sort_value(&self) -> &TokenIndex {
        match self.1 {
            SortIndex::Start => &self.0.start,
            SortIndex::End => &self.0.end,
        }
    }
}

impl PartialEq for IndexSort<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.sort_value() == other.sort_value()
    }
}
impl Eq for IndexSort<'_> { }

impl PartialOrd for IndexSort<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(TokenIndex::cmp(&self.sort_value(), &other.sort_value()))
    }
}

impl Ord for IndexSort<'_> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        IndexSort::partial_cmp(self, other).unwrap()
    }
}

// Errors

pub type ErrorKind = SymbolResolutionErrorKind;

#[derive(Debug)]
pub enum SymbolResolutionErrorKind {
    IOError,    // hit an io::Error while grabbing the symbol text
    EOFReached, // hit EOF before reaching the indicated end of symbol
}

#[derive(Debug)]
pub struct SymbolResolutionError {
    kind: ErrorKind,
    cause: Option<Box<dyn std::error::Error>>,
}

impl SymbolResolutionError {
    pub fn new(kind: ErrorKind) -> Self {
        SymbolResolutionError {
            kind, cause: None,
        }
    }
    
    pub fn caused_by(kind: ErrorKind, error: impl std::error::Error + 'static) -> Self {
        SymbolResolutionError {
            kind, cause: Some(Box::new(error)),
        }
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
}

impl From<ErrorKind> for SymbolResolutionError {
    fn from(kind: ErrorKind) -> Self {
        Self::new(kind)
    }
}

impl std::error::Error for SymbolResolutionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl fmt::Display for SymbolResolutionError {
    fn fmt(&self, _fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}