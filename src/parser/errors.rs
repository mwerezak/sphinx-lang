use std::fmt;
use std::error::Error;
use crate::utils;
use crate::source::ModuleSource;
use crate::lexer::{Span, TokenMeta, LexerError};
use crate::debug::symbol::{DebugSymbol, TokenIndex};


pub type ErrorKind = ParserErrorKind;

// Specifies the actual error that occurred
#[derive(Debug)]
pub enum ParserErrorKind {
    LexerError,
    EndofTokenStream,
    ExpectedStartOfExpr,   // expected the start of an expression
    ExpectedCloseParen,
    ExpectedCloseSquare,
    ExpectedCloseBrace,
    ExpectedIdentifier,
    ExpectedItemAfterLabel,   // expected either begin, while, for after label
    InvalidAssignment,      // the LHS of an assignment was not a valid lvalue
    InvalidDeclAssignment,  // only "=" allowed in variable decls
    DeclMissingInitializer, 
}

// Provide information about the type of syntactic construct from which the error originated
#[derive(Debug, Clone, Copy)]
pub enum ContextTag {
    Token,  // errors retrieving the actual tokens
    TopLevel,
    Sync,
    Stmt,
    Expr,
    BlockExpr,
    AssignmentExpr,
    VarDeclExpr,
    BinaryOpExpr,
    UnaryOpExpr,
    PrimaryExpr,
    MemberAccess,
    IndexAccess,
    ObjectCtor,
    TupleCtor,
    Atom,
    Group,
    Label,
}

// Since ErrorContext can share references with the Parser, we need to use 
// an error type that does not refer to the error context internally.
// The error context is always available at the base of the recursive descent call stack and can be added later.
#[derive(Debug)]
pub struct ErrorPrototype {
    kind: ErrorKind,
    lexer_error: Option<LexerError>,
}

impl ErrorPrototype {
    pub fn kind(&self) -> &ErrorKind { &self.kind }
}

impl From<ParserErrorKind> for ErrorPrototype {
    fn from(kind: ParserErrorKind) -> Self {
        ErrorPrototype { kind, lexer_error: None }
    }
}

impl From<LexerError> for ErrorPrototype {
    fn from(error: LexerError) -> Self {
        ErrorPrototype { 
            kind: ErrorKind::LexerError, 
            lexer_error: Some(error) 
        }
    }
}

#[derive(Debug)]
pub struct ParserError<'m> {
    kind: ErrorKind,
    module: &'m ModuleSource,
    context: ContextTag,
    symbol: DebugSymbol,
    cause: Option<Box<dyn Error>>,
}

impl<'m> ParserError<'m> {
    pub fn from_prototype(proto: ErrorPrototype, context: ErrorContext<'m>) -> Self {
        let module = context.module;
        let context_tag = context.frame().context();
        
        let symbol;
        let cause;
        if let Some(error) = proto.lexer_error {
            symbol = (&error.span).into();
            cause = Some(Box::new(error) as Box<dyn Error>);
        } else {
            symbol = context.take_debug_symbol();
            cause = None;
        }
        
        ParserError {
            module, cause, symbol,
            kind: proto.kind,
            context: context_tag,
        }
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
    pub fn module(&self) -> &ModuleSource { self.module }
    pub fn context(&self) -> &ContextTag { &self.context }
    pub fn debug_symbol(&self) -> &DebugSymbol { &self.symbol }
}


impl Error for ParserError<'_> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl fmt::Display for ParserError<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        
        let message = match self.kind() {
            ErrorKind::LexerError => "",
            ErrorKind::EndofTokenStream => "unexpected end of token stream",
            ErrorKind::ExpectedStartOfExpr  => "expected an expression here",
            ErrorKind::ExpectedCloseParen   => "missing closing ')'",
            ErrorKind::ExpectedCloseSquare  => "missing closing ']'",
            ErrorKind::ExpectedCloseBrace   => "missing closing '}'",
            ErrorKind::ExpectedIdentifier   => "invalid identifier",
            ErrorKind::InvalidAssignment    => "invalid assignment",
            ErrorKind::InvalidDeclAssignment  => "only '=' is allowed when initializing a newly declared variable",
            ErrorKind::DeclMissingInitializer => "missing '=' initializer for variable declaration",
            ErrorKind::ExpectedItemAfterLabel => "expected loop statement or block expression after label",
        };
        
        utils::format_error(fmt, "syntax error", Some(message), self.source())
    }
}


// Structures used by the parser for error handling and synchronization

#[derive(Debug, Clone)]
pub struct ErrorContext<'m> {
    module: &'m ModuleSource,
    stack: Vec<ContextFrame>,
}

impl<'m> ErrorContext<'m> {
    pub fn new(module: &'m ModuleSource, base: ContextTag) -> Self {
        ErrorContext {
            module, stack: vec![ ContextFrame::new(base) ],
        }
    }
    
    //pub fn module(&self) -> &'m str { self.module }
    
    pub fn frame(&self) -> &ContextFrame { self.stack.last().unwrap() }
    pub fn frame_mut(&mut self) -> &mut ContextFrame { self.stack.last_mut().unwrap() }
    
    pub fn push(&mut self, tag: ContextTag) { self.stack.push(ContextFrame::new(tag)) }
    
    pub fn push_continuation(&mut self, tag: ContextTag) {
        let start = self.frame().start().map(|o| o.to_owned());
        self.push(tag);
        self.frame_mut().set_span(start, None);
    }
    
    pub fn pop(&mut self) -> ContextFrame { 
        assert!(self.stack.len() > 1);
        self.stack.pop().unwrap()
    }
    
    pub fn pop_extend(&mut self) {
        let inner_frame = self.pop();
        self.frame_mut().extend(inner_frame);
    }
    
    pub fn take(mut self) -> ContextFrame {
        assert!(!self.stack.is_empty());
        self.stack.pop().unwrap()
    }
    
    // for convenience
    pub fn context(&self) -> ContextTag { self.frame().context() }
    pub fn set_start(&mut self, token: &TokenMeta) { self.frame_mut().set_start(token) }
    pub fn set_end(&mut self, token: &TokenMeta) { self.frame_mut().set_end(token) }
    
    pub fn take_debug_symbol(mut self) -> DebugSymbol {
        let mut symbol = self.frame().as_debug_symbol();
        while symbol.is_none() {
            if self.stack.len() <= 1 {
                symbol = self.take().as_debug_symbol();
                break;
            }
            
            self.pop();
            symbol = self.frame().as_debug_symbol();
        }
        
        symbol.expect("could not take debug symbol")
    }
}

#[derive(Debug, Clone)]
pub struct ContextFrame {
    tag: ContextTag,
    start: Option<Span>,
    end: Option<Span>,
}

fn span_lt(first: &Span, second: &Span) -> bool { first.index < second.index }
// fn span_min<'m>(first: &'m Span, second: &'m Span) -> &'m Span {
//     if span_lt(first, second) { first } else { second }
// }
// fn span_max<'m>(first: &'m Span, second: &'m Span) -> &'m Span {
//     if !span_lt(first, second) { first } else { second }
// }

impl ContextFrame {
    pub fn new(tag: ContextTag) -> Self { ContextFrame { tag, start: None, end: None } }
    
    pub fn context(&self) -> ContextTag { self.tag }
    pub fn start(&self) -> Option<&Span> { self.start.as_ref() }
    pub fn end(&self) -> Option<&Span> { self.end.as_ref() }
    
    pub fn set_start(&mut self, token: &TokenMeta) { 
        self.start.replace(token.span.clone()); 
    }
    
    pub fn set_end(&mut self, token: &TokenMeta) { 
        self.end.replace(token.span.clone()); 
    }
    
    pub fn set_span(&mut self, start: Option<Span>, end: Option<Span>) {
        self.start = start;
        self.end = end;
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
    
    pub fn as_debug_symbol(&self) -> Option<DebugSymbol> {
        match (self.start.clone(), self.end.clone()) {
            
            (Some(start), Some(end)) => {
                let start_index = start.index;
                let end_index = end.index + TokenIndex::from(end.length);
                Some((start_index, end_index).into())
            },
            
            (Some(span), None) | (None, Some(span)) => {
                Some((&span).into())
            },
            
            (None, None) => None,
        }
    }
}


