use log;
use core::fmt;
use std::error::Error;
use crate::utils;
use crate::lexer::{TokenMeta, LexerError};
use crate::debug::SourceError;
use crate::debug::symbol::DebugSymbol;


pub type ParseResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub enum ErrorKind {
    LexerError,
    EndofTokenStream,
    SyntaxError(String),
}

impl<S> From<S> for ErrorKind where S: ToString {
    fn from(message: S) -> Self {
        ErrorKind::SyntaxError(message.to_string())
    }
}

// Provide information about the type of syntactic construct from which the error originated
#[derive(Debug, Clone, Copy)]
pub enum ContextTag {
    Token,  // errors retrieving the actual tokens
    TopLevel,
    Sync,
    StmtMeta,
    StmtList,
    ControlFlow,
    Loop,
    WhileLoop,
    ExprMeta,
    ExprList,
    Expr,
    BlockExpr,
    IfExpr,
    FunDefExpr,
    FunParam,
    AssignmentExpr,
    BinaryOpExpr,
    UnaryOpExpr,
    PrimaryExpr,
    MemberAccess,
    IndexAccess,
    Invocation,
    ObjectCtor,
    TupleCtor,
    Atom,
    Group,
    LValue,
    Label,
}

impl From<ErrorKind> for ParserError {
    fn from(kind: ErrorKind) -> Self {
        Self { 
            kind, context: None, symbol: None, cause: None,
        }
    }
}

impl From<&str> for ParserError {
    fn from(message: &str) -> Self {
        Self { 
            kind: message.into(), 
            context: None, symbol: None, cause: None,
        }
    }
}

impl From<LexerError> for ParserError {
    fn from(error: LexerError) -> Self {
        Self { 
            kind: ErrorKind::LexerError, 
            context: None,
            symbol: Some(*error.debug_symbol()),
            cause: Some(Box::new(error)),
        }
    }
}

#[derive(Debug)]
pub struct ParserError {
    kind: ErrorKind,
    context: Option<ContextTag>,
    symbol: Option<DebugSymbol>,
    cause: Option<Box<dyn Error>>,
}

impl ParserError {
    pub fn with_context_tag(mut self, context: ContextTag) -> Self {
        self.context.get_or_insert(context); self
    }
    
    pub fn with_symbol(mut self, symbol: DebugSymbol) -> Self {
        self.symbol.get_or_insert(symbol); self
    }
    
    pub fn with_symbol_from_ctx(mut self, ctx: &ErrorContext) -> Self {
        if let Some(symbol) = ctx.frame().as_debug_symbol() {
            self.symbol.replace(symbol);
        }
        self
    }
    
    pub fn with_cause(mut self, error: impl Error + 'static) -> Self {
        self.cause.replace(Box::new(error)); self
    }
    
    // fill in fields from context if not already set
    pub fn with_error_context(mut self, context: ErrorContext) -> Self {
        if self.context.is_none() {
            self.context.replace(context.frame().context());
        }
        if self.symbol.is_none() {
            self.symbol.replace(context.take_debug_symbol());
        }
        self
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
    pub fn context(&self) -> Option<&ContextTag> { self.context.as_ref() }
}


impl Error for ParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl SourceError for ParserError {
    fn debug_symbol(&self) -> Option<&DebugSymbol> { self.symbol.as_ref() }
}

impl fmt::Display for ParserError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        
        let message = match self.kind() {
            ErrorKind::LexerError => "",
            ErrorKind::EndofTokenStream => "unexpected end of token stream",
            ErrorKind::SyntaxError(message) => message,
        };
        
        utils::format_error(fmt, "Syntax error", Some(message), self.source())
    }
}
// "unpacking may only be used once in an assignment or declaration"

// Structures used by the parser for error handling and synchronization

#[derive(Debug)]
pub struct ErrorContext {
    stack: Vec<ContextFrame>,
}

impl<'m> ErrorContext {
    pub fn new(base: ContextTag) -> Self {
        ErrorContext {
            stack: vec![ ContextFrame::new(base) ],
        }
    }
    
    pub fn frame(&self) -> &ContextFrame { 
        self.stack.last().unwrap() 
    }
    
    pub fn frame_mut(&mut self) -> &mut ContextFrame { 
        self.stack.last_mut().unwrap() 
    }
    
    pub fn push(&mut self, tag: ContextTag) { 
        log::debug!("Push frame: {:0>3} {:?}", self.stack.len()+1, tag);
        self.stack.push(ContextFrame::new(tag)) 
    }
    
    // treat the prev top frame as if it was *above* the new top frame
    pub fn push_continuation(&mut self, tag: ContextTag, frame: Option<ContextFrame>) {
        // log::debug!("Set frame:  {:0>3} {:?} -> {:?}", self.stack.len(), self.frame().context(), tag);
        let frame = frame.unwrap_or_else(|| self.frame().clone());
        self.push(tag);
        self.frame_mut().extend(frame);
    }
    
    pub fn pop(&mut self) -> ContextFrame { 
        log::debug!("Pop frame:  {:0>3} {:?}", self.stack.len(), self.frame().context());
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
    start: Option<DebugSymbol>,
    end: Option<DebugSymbol>,
}


impl ContextFrame {
    pub fn new(tag: ContextTag) -> Self { ContextFrame { tag, start: None, end: None } }
    
    pub fn context(&self) -> ContextTag { self.tag }
    pub fn start(&self) -> Option<&DebugSymbol> { self.start.as_ref() }
    pub fn end(&self) -> Option<&DebugSymbol> { self.end.as_ref() }
    
    pub fn set_start(&mut self, token: &TokenMeta) { 
        self.start.replace(token.symbol); 
    }
    
    pub fn set_end(&mut self, token: &TokenMeta) { 
        self.end.replace(token.symbol); 
    }
    
    pub fn set_span(&mut self, start: Option<DebugSymbol>, end: Option<DebugSymbol>) {
        self.start = start;
        self.end = end;
    }
    
    pub fn extend(&mut self, other: ContextFrame) {
        let spans = [self.start, other.start, self.end, other.end];
        let start = spans.iter().filter_map(|s| s.as_ref())
            .min_by(|a, b| a.start().cmp(&b.start()));
        
        let end = spans.iter().filter_map(|s| s.as_ref())
            .max_by(|a, b| a.end().cmp(&b.end()));
        
        self.start = start.copied();
        self.end = end.copied();
    }
    
    pub fn as_debug_symbol(&self) -> Option<DebugSymbol> {
        match (self.start, self.end) {
            
            (Some(start), Some(end)) => {
                (start.start(), end.end()).try_into().ok()
            },
            
            (Some(symbol), None) | (None, Some(symbol)) => {
                Some(symbol)
            },
            
            (None, None) => None,
        }
    }
}


