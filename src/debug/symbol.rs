use std::rc::Rc;
use crate::lexer::Span;

pub use crate::lexer::{TokenIndex, TokenLength};

pub mod table;
pub mod resolver;
pub mod errors;

pub use table::{ChunkSymbols, DebugSymbolTable};
pub use resolver::{DebugSymbolResolver, ResolvedSymbolTable};


/// When provided along with the source text, identifies a span of source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl From<&Span> for DebugSymbol {
    fn from(span: &Span) -> Self {
        let start = span.index;
        let end = span.index + TokenIndex::from(span.length);
        DebugSymbol { start, end }
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
