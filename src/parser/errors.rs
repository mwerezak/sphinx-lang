use std::fmt;
use std::error::Error;
use crate::lexer::{Span, TokenMeta};
use crate::parser::debug::DebugMeta;


// Box any owned TokenMeta to prevent bloat
#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    RanOutOfTokens,
    LexerError,
    ExpectedExpr,
    ExpectedAtom,
    ExpectedGroupClose,
    ExpectedIndexingClose,
    ExpectedObjectCtorClose,
    ExpectedAccessIdentifier,  // expected an identifier following a "."
    ExpectedVarAssignment,     // expected an assignment following "var"
    InvalidAssignmentLHS,
}

#[derive(Debug)]
pub struct ParserError {
    kind: ErrorKind,
    cause: Option<Box<dyn Error>>,
}

impl ParserError {
    pub fn new(kind: ErrorKind) -> Self {
        ParserError {
            kind, cause: None,
        }
    }
    
    pub fn caused_by(error: Box<dyn Error>, kind: ErrorKind) -> Self {
        ParserError {
            kind, cause: Some(error),
        }
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
}

impl Error for ParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, _fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}




// Structures used by the parser for error handling and synchronization (when I get there)

pub struct ErrorContext {
    stack: Vec<ContextFrame>,
}

impl ErrorContext {
    pub fn new() -> Self {
        ErrorContext { stack: vec![ ContextFrame::new() ] }
    }
    
    pub fn frame(&self) -> &ContextFrame { self.stack.last().unwrap() }
    pub fn frame_mut(&mut self) -> &mut ContextFrame { self.stack.last_mut().unwrap() }
    
    pub fn push(&mut self) { self.stack.push(ContextFrame::new()) }
    
    pub fn pop(&mut self) -> ContextFrame { 
        debug_assert!(self.stack.len() > 1);
        self.stack.pop().unwrap()
    }
    
    // pops the current context frame and merges it by updating the end span
    pub fn pop_extend(&mut self) -> ContextFrame {
        debug_assert!(self.stack.len() > 1);
        let frame = self.pop();
        self.frame_mut().extend(&frame);
        
        frame
    }
    
    pub fn reset(&mut self) { 
        self.stack.clear();
        self.push();
    }
    
    // for convenience
    pub fn set_start(&mut self, token: &TokenMeta) { self.frame_mut().set_start(token) }
    pub fn set_end(&mut self, token: &TokenMeta) { self.frame_mut().set_end(token) }
    pub fn set_span(&mut self, start: &TokenMeta, end: &TokenMeta) { self.frame_mut().set_span(start, end) }
}

pub struct ContextFrame {
    start: Option<Span>,
    end: Option<Span>,
}

fn span_lt(first: &Span, second: &Span) -> bool { first.index < second.index }
fn span_min<'a>(first: &'a Span, second: &'a Span) -> &'a Span {
    if span_lt(first, second) { first } else { second }
}
fn span_max<'a>(first: &'a Span, second: &'a Span) -> &'a Span {
    if !span_lt(first, second) { first } else { second }
}

impl ContextFrame {
    pub fn new() -> Self { ContextFrame { start: None, end: None } }
    
    pub fn set_start(&mut self, token: &TokenMeta) { 
        self.start.replace(token.span); 
    }
    
    pub fn set_end(&mut self, token: &TokenMeta) { 
        self.end.replace(token.span); 
    }
    
    pub fn set_span(&mut self, start: &TokenMeta, end: &TokenMeta) {
        self.set_start(start);
        self.set_end(end);
    }
    
    pub fn extend(&mut self, other: &ContextFrame) {
        if self.start.and(other.start).is_some() {
            let earliest = span_min(&self.start.unwrap(), &other.start.unwrap());
            self.start.replace(*earliest);
        } else {
            self.start = self.start.or(other.start);
        }
        
        if self.end.and(other.end).is_some() {
            let latest = span_max(&self.end.unwrap(), &other.end.unwrap());
            self.end.replace(*latest);
        } else {
            self.end = self.end.or(other.end);
        }
    }
    
    pub fn dbg_info<'n>(&self, file: &'n str) -> DebugMeta<'n> {
        DebugMeta {
            file,
            start: self.start,
            end: self.end,
        }
    }
}