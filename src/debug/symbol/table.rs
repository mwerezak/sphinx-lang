use std::cmp::Ordering;
use std::collections::HashMap;
use crate::codegen::ChunkID;
use crate::debug::symbol::DebugSymbol;

pub type ChunkSymbols = HashMap<Option<ChunkID>, DebugSymbolTable>;

/// Maps bytecode offsets to DebugSymbols
#[derive(Debug)]
pub struct DebugSymbolTable {
    entries: Vec<SymbolTableEntry>,
}

impl Default for DebugSymbolTable {
    fn default() -> Self { Self::new() }
}

impl DebugSymbolTable {
    pub fn new() -> Self {
        Self { entries: Vec::new() }
    }
    
    pub fn insert(&mut self, offset: usize, symbol: DebugSymbol) {
        let entry = SymbolTableEntry(offset, symbol);
        
        if matches!(self.entries.last(), Some(last_entry) if entry <= *last_entry) {
            panic!("symbol inserted out of order");
        }
        
        self.entries.push(entry)
    }
    
    pub fn lookup(&self, offset: usize) -> Option<&DebugSymbol> {
        if let Ok(index) = self.entries.binary_search_by_key(&offset, SymbolTableEntry::offset) {
            Some(&self.entries[index].1)
        } else {
            None
        }
    }
    
    pub fn iter(&self) -> impl Iterator<Item=(usize, &DebugSymbol)> + '_ {
        self.entries.iter().map(|entry| {
            let SymbolTableEntry(offset, symbol) = entry;
            (*offset, symbol)
        })
    }
    
    pub fn symbols(&self) -> impl Iterator<Item=&DebugSymbol> {
        self.entries.iter().map(|entry| &entry.1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SymbolTableEntry(usize, DebugSymbol);

impl PartialOrd for SymbolTableEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        usize::partial_cmp(&self.0, &other.0)
    }
}

impl Ord for SymbolTableEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        usize::cmp(&self.0, &other.0)
    }
}

impl SymbolTableEntry {
    fn offset(&self) -> usize { self.0 }
}