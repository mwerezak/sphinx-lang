pub use crate::lexer::TokenIndex;

use crate::source::ModuleSource;
use crate::runtime::bytecode::Chunk;


// metadata attached to parser output for error handling and debug output
// will probably be attached to the statement level


#[derive(Debug, Clone)]
pub struct DebugSymbol {
    pub start: TokenIndex,
    pub end: TokenIndex,
}

impl DebugSymbol {
    pub fn new(start: TokenIndex, end: TokenIndex) -> Self {
        DebugSymbol { start, end }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedSymbol {
    pub line: String,
    pub lineno: usize,
    pub start: usize,  // start,end indices into line
    pub end: usize,
}


// Container for debug symbols generated for bytecode
// Should contain a DebugSymbol for each opcode in the 
// associated Chunk, and in the same order.
#[derive(Clone)]
pub struct ChunkDebugSymbols {
    source: ModuleSource,
    symbols: Vec<DebugSymbol>,
}

impl ChunkDebugSymbols {
    pub fn new(source: ModuleSource) -> Self {
        ChunkDebugSymbols {
            source, symbols: Vec::new(),
        }
    }
    
    pub fn symbols(&self) -> &[DebugSymbol] { &self.symbols }
    
    pub fn push(&mut self, symbol: DebugSymbol) {
        self.symbols.push(symbol)
    }
}

pub trait DebugSymbolResolver {
    fn resolve_symbols<S, R>(&self, symbols: S) -> R 
    where S: Iterator<Item=DebugSymbol>, R: Iterator<Item=ResolvedSymbol>;
}

impl DebugSymbolResolver for ModuleSource {
    fn resolve_symbols<S, R>(&self, symbols: S) -> R 
    where S: Iterator<Item=DebugSymbol>, R: Iterator<Item=ResolvedSymbol> {
        unimplemented!()
    }
}