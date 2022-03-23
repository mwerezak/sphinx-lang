use std::fmt;
use std::error::Error;

use crate::utils;
use crate::debug::{DebugSymbol, SourceError};


pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub enum ErrorKind {
    ChunkCountLimit,
    ConstPoolLimit,
    ParamCountLimit,
    ArgCountLimit,
    TupleLengthLimit,
    LocalVariableLimit,
    CalcJumpOffsetFailed,
    CantAssignImmutable,
    CantAssignNonLocal,
    Other(String),
}

impl<S> From<S> for ErrorKind where S: ToString {
    fn from(message: S) -> Self {
        ErrorKind::Other(message.to_string())
    }
}

#[derive(Debug)]
pub struct CompileError {
    kind: ErrorKind,
    symbol: Option<DebugSymbol>,
    cause: Option<Box<dyn Error>>,
}

impl CompileError {
    pub fn new<S>(message: S) -> Self where S: ToString {
        ErrorKind::from(message).into()
    }
    
    pub fn with_symbol(mut self, symbol: DebugSymbol) -> Self {
        self.symbol.get_or_insert(symbol); self 
    }
    
    pub fn caused_by(mut self, error: impl Error + 'static) -> Self {
        self.cause.replace(Box::new(error)); self
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
}

impl From<ErrorKind> for CompileError {
    fn from(kind: ErrorKind) -> Self {
        Self { kind, symbol: None, cause: None }
    }
}

impl Error for CompileError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl SourceError for CompileError {
    fn debug_symbol(&self) -> Option<&DebugSymbol> { self.symbol.as_ref() }
}

impl fmt::Display for CompileError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        
        let message = match self.kind() {
            // Limit exceeded errors
            ErrorKind::ChunkCountLimit => "chunk count limit reached",
            ErrorKind::ConstPoolLimit => "constant pool limit reached",
            ErrorKind::ParamCountLimit => "parameter limit exceeded",
            ErrorKind::ArgCountLimit => "argument limit exceeded",
            ErrorKind::TupleLengthLimit => "tuple length limit exceeded",
            ErrorKind::LocalVariableLimit => "local variable limit reached",
            ErrorKind::CalcJumpOffsetFailed => "could not calculate jump offset",
            
            // Actual user errors
            ErrorKind::CantAssignImmutable => "can't assign to immutable local variable",
            ErrorKind::CantAssignNonLocal => "can't assign to a non-local variable without the \"nonlocal\" keyword",
            ErrorKind::Other(message) => message,
        };
        
        utils::format_error(fmt, "compile error", Some(message), self.source())
    }
}