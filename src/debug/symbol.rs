pub use crate::lexer::{TokenIndex, TokenLength};

use std::fmt;
use std::io;
use std::cmp;
use std::collections::{BinaryHeap, HashMap};
use crate::source::{ModuleSource, SourceText};
use crate::runtime::bytecode::Chunk;


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
        self.symbols.iter().flat_map(|(sym, count)| std::iter::repeat(sym).take(usize::from(*count)))
    }
    
    pub fn push(&mut self, symbol: DebugSymbol) {
        match self.symbols.last_mut() {
            Some((last, ref mut count)) if *last == symbol && *count < u8::MAX => { 
                *count += 1 
            },
            _ => { self.symbols.push((symbol, 1)) }
        }
        
        
        
        // if matches!(self.symbols.last(), Some((last, ref mut count)) if *last == symbol && *count < u8::MAX) {
        //     count += 1;
        // } else {
        //     self.symbols.push((symbol, 1));
        // }
    }
}

// Resolved Symbols

#[derive(Debug, Clone)]
pub struct ResolvedSymbol {
    text: String,
    lineno: usize,  // line number at the start of the symbol
    start: usize,   // start,end indices into self.text
    end: usize,
    
    // indices to chars in self.text at the start of each line, except the first
    // if this is a single-line symbol then this will be empty
    lines: Vec<usize>, 
}

impl ResolvedSymbol {
    pub fn new(text: String, lineno: usize, start: usize, end: usize) -> Self {
        let mut lines = Vec::new();
        for (index, c) in text.chars().enumerate() {
            if c == '\n' && index + 1 < text.len() {
                lines.push(index + 1);
            }
        }
        
        ResolvedSymbol { text, lineno, start, end, lines }
    }
    
    pub fn is_multiline(&self) -> bool { !self.lines.is_empty() }
    
    pub fn symbol_text(&self) -> &str { 
        &self.text[self.start..self.end]
    }
    
    // includes surrounding text on the same lines
    pub fn entire_text(&self) -> &str {
        &self.text
    }
    
    // iterate the lines of a multiline symbol WITHOUT the newline characters
    pub fn iter_lines(&self) -> impl Iterator<Item=&str> { 
        IterLines { 
            text: self.text.as_str(),
            newlines: self.lines.iter().copied(),
            prev: 0
        }
    }
    
    // returns a Display that will produce an abbreviated version of the symbol showing just the first line
    // if the symbol isn't multiline, the result will be the same as format!("{}", self.symbol())
    pub fn as_single_line_fmt(&self) -> impl fmt::Display + '_ {
        SingleLineFormat { symbol: &self }
    }
}

impl fmt::Display for ResolvedSymbol {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(self.symbol_text())
    }
}

struct IterLines<'s, I> where I: Iterator<Item=usize> {
    text: &'s str,
    newlines: I,
    prev: usize,
}

impl<'s, I> Iterator for IterLines<'s, I> where I: Iterator<Item=usize> {
    type Item = &'s str;
    fn next(&mut self) -> Option<Self::Item> {
        let line_end = self.newlines.next()?;
        let line_start = self.prev;
        self.prev = line_end;
        
        Some(&self.text[line_start..line_end - 1]) //drop the terminating newline
    }
}

struct SingleLineFormat<'s> {
    symbol: &'s ResolvedSymbol,
}

impl fmt::Display for SingleLineFormat<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(line) = self.symbol.iter_lines().nth(0) {
            write!(fmt, "{}...", line)
        } else {
            fmt.write_str(self.symbol.symbol_text())
        }
    }
}

// Resolved Symbol Formatting

// Symbol Resolution

pub type ResolvedSymbolTable = HashMap<DebugSymbol, Result<ResolvedSymbol, SymbolResolutionError>>;

pub trait DebugSymbolResolver {
    fn resolve_symbols<S>(&self, symbols: S) -> io::Result<ResolvedSymbolTable> where S: Iterator<Item=DebugSymbol>;
}

impl DebugSymbolResolver for ModuleSource {
    fn resolve_symbols<S>(&self, symbols: S) -> io::Result<ResolvedSymbolTable> where S: Iterator<Item=DebugSymbol> {
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

fn resolve_debug_symbols(source: impl Iterator<Item=io::Result<char>>, symbols: impl Iterator<Item=DebugSymbol>) -> ResolvedSymbolTable {
    // put all the symbols into a priority queue based on first occurrence in the source text
    let mut next_symbols = BinaryHeap::new();
    next_symbols.extend(symbols.map(|sym| IndexSort(sym, SortIndex::Start)).map(cmp::Reverse));
    
    let mut open_symbols = BinaryHeap::new();
    let mut active_symbols = HashMap::<DebugSymbol, (String, usize, usize)>::new(); // values are (buffer, line number, start index)
    let mut closing_symbols = HashMap::<DebugSymbol, (String, usize, usize, usize)>::new();
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
            
                for (symbol, (strbuf, lineno, start_index, end_index)) in closing_symbols.drain() {
                    let resolved = ResolvedSymbol::new(strbuf + "...???", lineno, start_index, end_index);
                    resolved_symbols.insert(symbol, Ok(resolved));
                }
            
                break;
            },
        };
        
        // add the char to symbols
        current_line.push(c);
        for (ref mut strbuf, ..) in active_symbols.values_mut() {
            strbuf.push(c);
        }
        for (ref mut strbuf, ..) in closing_symbols.values_mut() {
            strbuf.push(c);
        }
        
        // if we are at the start of a new symbol, open it
        while matches!(next_symbols.peek(), Some(&cmp::Reverse(IndexSort(ref sym,..))) if index == sym.start) {
            let symbol = next_symbols.pop().unwrap().0.0;
            if !active_symbols.contains_key(&symbol) {
                open_symbols.push(cmp::Reverse(IndexSort(symbol.clone(), SortIndex::End)));
                
                let start_index = current_line.len() - 1;
                let symbol_data = (current_line.clone(), lineno, start_index); 
                active_symbols.insert(symbol, symbol_data);
            }
        }
        
        // if we are at the end of an open symbol, close it
        while matches!(open_symbols.peek(), Some(&cmp::Reverse(IndexSort(ref sym,..))) if index == sym.end) {
            let symbol = open_symbols.pop().unwrap().0.0;
            if let Some((strbuf, lineno, start_index)) = active_symbols.remove(&symbol) {
                if !closing_symbols.contains_key(&symbol) {
                    let end_index = strbuf.len();
                    closing_symbols.insert(symbol, (strbuf, lineno, start_index, end_index));
                }
            }
        }
        
        // finalize all symbols in closing_symbols at the end of the current line
        if c == '\n' {
            lineno +=1; // count newlines
            current_line.clear();
            for (symbol, (strbuf, lineno, start_index, end_index)) in closing_symbols.drain() {
                let resolved = ResolvedSymbol::new(strbuf, lineno, start_index, end_index);
                resolved_symbols.insert(symbol, Ok(resolved));
            }
        }
        
        if next_symbols.is_empty() && active_symbols.is_empty() && closing_symbols.is_empty() {
            break;
        }
    }
    
    resolved_symbols
}


enum SortIndex { Start, End }

// comparison based on start or end index
struct IndexSort(DebugSymbol, SortIndex);

impl IndexSort {
    fn sort_value(&self) -> &TokenIndex {
        match self.1 {
            SortIndex::Start => &self.0.start,
            SortIndex::End => &self.0.end,
        }
    }
}

impl PartialEq for IndexSort {
    fn eq(&self, other: &Self) -> bool {
        self.sort_value() == other.sort_value()
    }
}
impl Eq for IndexSort { }

impl PartialOrd for IndexSort {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(TokenIndex::cmp(&self.sort_value(), &other.sort_value()))
    }
}

impl Ord for IndexSort {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        IndexSort::partial_cmp(self, other).unwrap()
    }
}

// Errors

pub type ErrorKind = SymbolResolutionErrorKind;

#[derive(Debug)]
pub enum SymbolResolutionErrorKind {
    IOError,
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
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}