use std::fmt;
use std::error::Error;
use crate::lexer::{Span, TokenMeta};
use crate::parser::debug::DebugMeta;

// Specifies the actual error that occurred
#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    RanOutOfTokens,
    LexerError,
    ExpectedExpr,   // expected the start of an expression
    ExpectedAtom,
    ExpectedCloseParen,
    ExpectedCloseSquare,
    ExpectedCloseBrace,
    ExpectedIdentifier,
    ExpectedAssignmentExpr, // expected an assignment expression
    InvalidAssignmentLHS,   // the LHS of an assignment was not a valid lvalue
}

// Provide information about the type of syntactic construct from which the error originated
#[derive(Clone, Copy, Debug)]
pub enum ContextTag {
    Token,  // errors retrieving the actual tokens
    Expr,
    PrimaryExpr,
    MemberAccess,
    IndexAccess,
    ObjectCtor,
    AssignmentExpr,
    Atom,
}


#[derive(Debug)]
pub struct ParserError {
    kind: ErrorKind,
    context: ContextTag,
    cause: Option<Box<dyn Error>>,
}

impl ParserError {
    pub fn new(kind: ErrorKind, context: ContextTag) -> Self {
        ParserError {
            kind, context, cause: None,
        }
    }
    
    pub fn caused_by(error: Box<dyn Error>, kind: ErrorKind, context: ContextTag) -> Self {
        ParserError {
            kind, context, cause: Some(error),
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
    pub fn new(base: ContextTag) -> Self {
        ErrorContext { stack: vec![ ContextFrame::new(base) ] }
    }
    
    pub fn frame(&self) -> &ContextFrame { self.stack.last().unwrap() }
    pub fn frame_mut(&mut self) -> &mut ContextFrame { self.stack.last_mut().unwrap() }
    
    pub fn push(&mut self, tag: ContextTag) { self.stack.push(ContextFrame::new(tag)) }
    
    pub fn pop(&mut self) -> ContextFrame { 
        debug_assert!(self.stack.len() > 1);
        self.stack.pop().unwrap()
    }
    
    // pops the current context frame and merges it by updating the end span
    // unlike pop(), does not return anything since the frame is consumed
    pub fn pop_extend(&mut self) {
        debug_assert!(self.stack.len() > 1);
        let frame = self.pop();
        self.frame_mut().extend(frame);
    }
    
    // for convenience
    pub fn context(&self) -> ContextTag { self.frame().context() }
    pub fn set_start(&mut self, token: &TokenMeta) { self.frame_mut().set_start(token) }
    pub fn set_end(&mut self, token: &TokenMeta) { self.frame_mut().set_end(token) }
    pub fn set_span(&mut self, start: &TokenMeta, end: &TokenMeta) { self.frame_mut().set_span(start, end) }
}

pub struct ContextFrame {
    tag: ContextTag,
    start: Option<Span>,
    end: Option<Span>,
}

fn span_lt(first: &Span, second: &Span) -> bool { first.index < second.index }
// fn span_min<'a>(first: &'a Span, second: &'a Span) -> &'a Span {
//     if span_lt(first, second) { first } else { second }
// }
// fn span_max<'a>(first: &'a Span, second: &'a Span) -> &'a Span {
//     if !span_lt(first, second) { first } else { second }
// }

impl ContextFrame {
    pub fn new(tag: ContextTag) -> Self { ContextFrame { tag, start: None, end: None } }
    
    pub fn context(&self) -> ContextTag { self.tag }
    
    pub fn set_start(&mut self, token: &TokenMeta) { 
        self.start.replace(token.span.clone()); 
    }
    
    pub fn set_end(&mut self, token: &TokenMeta) { 
        self.end.replace(token.span.clone()); 
    }
    
    pub fn set_span(&mut self, start: &TokenMeta, end: &TokenMeta) {
        self.set_start(start);
        self.set_end(end);
    }
    
    pub fn extend(&mut self, other: ContextFrame) {
        if self.start.as_ref().and(other.start.as_ref()).is_some() {
            if span_lt(other.start.as_ref().unwrap(), self.start.as_ref().unwrap()) {
                self.start = other.start;
            }
        } else if other.start.is_some() {
            self.start = other.start;
        }
        
        if self.end.as_ref().and(other.end.as_ref()).is_some() {
            if span_lt(self.end.as_ref().unwrap(), other.end.as_ref().unwrap()) {
                self.end = other.end;
            }
        } else if other.end.is_some() {
            self.end = other.end;
        }
    }
    
    pub fn to_dbg_meta<'n>(self, file: &'n str) -> DebugMeta<'n> {
        DebugMeta {
            file,
            start: self.start,
            end: self.end,
        }
    }
}