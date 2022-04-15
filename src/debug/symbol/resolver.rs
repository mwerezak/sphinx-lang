// Symbol Resolution

use core::cmp;
use core::iter;

use std::io;
use std::rc::Rc;
use std::collections::{BinaryHeap, HashMap};

use crate::source::{ModuleSource, SourceText};
use crate::debug::symbol::{DebugSymbol, ResolvedSymbol, TokenIndex};
use crate::debug::symbol::errors::{SymbolResolutionError, ErrorKind};


pub trait DebugSymbolResolver {
    fn resolve_symbols<'s, S>(&self, symbols: S) -> io::Result<ResolvedSymbolTable<'s>> where S: Iterator<Item=&'s DebugSymbol>;
}


impl DebugSymbolResolver for ModuleSource {
    fn resolve_symbols<'s, S>(&self, symbols: S) -> io::Result<ResolvedSymbolTable<'s>> where S: Iterator<Item=&'s DebugSymbol> {
        match self.read_text()? {
            SourceText::String(text) => Ok(resolve_debug_symbols(text.chars().map(Ok), symbols)),
            SourceText::File(text) => Ok(resolve_debug_symbols(text, symbols))
        }
    }
}

pub struct BufferedResolver {
    buffer: String,
}

impl BufferedResolver {
    pub fn new(string: impl ToString) -> Self {
        Self { buffer: string.to_string() }
    }
}

impl DebugSymbolResolver for BufferedResolver {
    fn resolve_symbols<'s, S>(&self, symbols: S) -> io::Result<ResolvedSymbolTable<'s>> where S: Iterator<Item=&'s DebugSymbol> {
        Ok(resolve_debug_symbols(self.buffer.chars().map(Ok), symbols))
    }
}


pub struct ResolvedSymbolTable<'s> {
    table: HashMap<&'s DebugSymbol, Result<ResolvedSymbol, SymbolResolutionError>>,
}

impl<'s> iter::FromIterator<(&'s DebugSymbol, Result<ResolvedSymbol, SymbolResolutionError>)> for ResolvedSymbolTable<'s> {
    fn from_iter<T>(iter: T) -> Self 
    where T: IntoIterator<Item=(&'s DebugSymbol, Result<ResolvedSymbol, SymbolResolutionError>)> {
        Self {
            table: HashMap::from_iter(iter)
        }
    }
}

impl<'s> ResolvedSymbolTable<'s> {
    pub fn lookup(&self, symbol: &DebugSymbol) -> Option<Result<&ResolvedSymbol, &SymbolResolutionError>> {
        self.table.get(symbol).map(|result| result.as_ref())
    }
    
    pub fn iter(&self) -> impl Iterator<Item=(&DebugSymbol, Result<&ResolvedSymbol, &SymbolResolutionError>)> {
        self.table.iter().map(|(symbol, resolved)| (*symbol, resolved.as_ref()))
    }
    
    fn new() -> Self {
        Self { table: HashMap::new() }
    }
    
    fn insert(&mut self, symbol: &'s DebugSymbol, result: Result<ResolvedSymbol, SymbolResolutionError>) {
        self.table.insert(symbol, result);
    }
}


// resolution procedure


fn resolve_debug_symbols<'s>(source: impl Iterator<Item=io::Result<char>>, symbols: impl Iterator<Item=&'s DebugSymbol>) -> ResolvedSymbolTable<'s> {
    // deduplicate symbols
    let mut dedup_symbols = symbols.collect::<Vec<&DebugSymbol>>();
    dedup_symbols.dedup();
    
    // put all the symbols into a priority queue based on first occurrence in the source text
    let mut next_symbols = BinaryHeap::new();
    next_symbols.extend(dedup_symbols.into_iter().map(|sym| IndexSort(sym, SortIndex::Start)).map(cmp::Reverse));
    // let symbol_count = next_symbols.len();
    
    let mut open_symbols = BinaryHeap::new();
    let mut active_symbols = HashMap::<&DebugSymbol, (Vec<Rc<String>>, usize, usize)>::new(); // values are (buffer, line number, start index)
    let mut closing_symbols = HashMap::<&DebugSymbol, (Vec<Rc<String>>, usize, usize, usize)>::new();
    let mut resolved_symbols = ResolvedSymbolTable::new();
    
    // handle symbols at EOF by adding a single blank at the end
    let source = source.chain(iter::once(Ok(' ')));
    
    let mut lineno = 1;  // count lines
    let mut current_line = String::new();
    for (char_result, index) in source.zip(0..) {
        // check for io::Errors 
        let c = match char_result {
            Ok(c) => c,
            
            Err(ioerror) => {
                let ioerror = Rc::new(ioerror);
                
                // drop all open symbols and close all closing symbols
                active_symbols.clear();
                for cmp::Reverse(IndexSort(symbol,..)) in open_symbols.drain() {
                    let error = SymbolResolutionError::caused_by(*symbol, ioerror.clone());
                    resolved_symbols.insert(symbol, Err(error));
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
        while matches!(next_symbols.peek(), Some(&cmp::Reverse(IndexSort(sym,..))) if index == sym.start()) {
            let symbol = next_symbols.pop().unwrap().0.0;
            
            active_symbols.entry(symbol).or_insert_with(|| {
                open_symbols.push(cmp::Reverse(IndexSort(symbol, SortIndex::End)));
                
                let start_index = current_line.len() - 1;
                (Vec::new(), lineno, start_index)
            });
        }
        
        // if we are at the end of an open symbol, mark it as closing
        while matches!(open_symbols.peek(), Some(&cmp::Reverse(IndexSort(sym,..))) if index == sym.end()) {
            let symbol = open_symbols.pop().unwrap().0.0;
            if let Some((lines, lineno, start_index)) = active_symbols.remove(&symbol) {
                closing_symbols.entry(symbol).or_insert_with(|| {
                    
                    // calculate end_index
                    let total_len = lines.iter()
                        .map(|line| line.len())
                        .reduce(|acc, n| acc+n)
                        .unwrap_or(0);
                    
                    let end_index = total_len + current_line.len() - 1;
                    
                    (lines, lineno, start_index, end_index)
                });
            }
        }
        
        // once we complete the current line, add it to all open and closing symbols
        if c == '\n' {
            lineno +=1; // count newlines
            let line_rc = Rc::new(current_line.clone());
            
            // open symbols
            for (ref mut lines, ..) in active_symbols.values_mut() {
                lines.push(Rc::clone(&line_rc));
            }
            
            // close all closing symbols
            for (symbol, (mut lines, lineno, start_index, end_index)) in closing_symbols.drain() {
                lines.push(Rc::clone(&line_rc));
                
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
        let error = SymbolResolutionError::new(*symbol, ErrorKind::EOFReached);
        resolved_symbols.insert(symbol, Err(error));
    }
    
    let line_rc = Rc::new(current_line.clone());
    for (symbol, (mut lines, lineno, start_index, end_index)) in closing_symbols.drain() {
        lines.push(line_rc.clone());
        
        let resolved = ResolvedSymbol::new(lines, lineno, start_index, end_index);
        resolved_symbols.insert(symbol, Ok(resolved));
    }
    
    // debug_assert!(symbol_count == resolved_symbols.len());
    
    resolved_symbols
}


#[derive(Debug)]
enum SortIndex { Start, End }

// comparison based on start or end index

#[derive(Debug)]
struct IndexSort<'s>(&'s DebugSymbol, SortIndex);

impl IndexSort<'_> {
    fn sort_value(&self) -> TokenIndex {
        match self.1 {
            SortIndex::Start => self.0.start(),
            SortIndex::End => self.0.end(),
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
