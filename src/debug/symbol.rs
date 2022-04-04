use std::rc::Rc;

pub mod table;
pub mod resolver;
pub mod errors;

pub use table::{ChunkSymbols, DebugSymbolTable};
pub use resolver::{DebugSymbolResolver, ResolvedSymbolTable};


// Max source file length ~4 billion characters (assuming mostly single byte UTF8 that's a ~4GB file)
// Max token length 65535 characters
pub type TokenIndex = u32;
pub type TokenLength = u16;


/// When provided along with the source text, identifies a span of source code.
/// Because they are passed around everywhere, the debug symbols used by Sphinx are very light weight.
/// Just an index into the source text and a length. In order to be useful they must first be resolved
/// using a `DebugSymbolResolver`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebugSymbol {
    start: TokenIndex,
    length: TokenLength,
}

impl TryFrom<(TokenIndex, TokenIndex)> for DebugSymbol {
    type Error = (TokenIndex, TokenIndex);
    fn try_from(tuple: (TokenIndex, TokenIndex)) -> Result<Self, Self::Error> {
        let (start, end) = tuple;
        if let Ok(length) = TokenLength::try_from(end - start) {
            Ok(DebugSymbol { start, length })
        } else {
            Err((start, end))
        }
    }
}

impl DebugSymbol {
    pub fn new(start: TokenIndex, length: TokenLength) -> Self {
        Self { start, length }
    }
    
    pub fn start(&self) -> TokenIndex { self.start }
    pub fn end(&self) -> TokenIndex { self.start + TokenIndex::from(self.length) }
    pub fn len(&self) -> TokenLength { self.length }
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
                result = &line;
            };
            
            *cur_line_start = cur_line_end;
            Some(result)
        })
    }

}
