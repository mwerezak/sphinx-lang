use std::fmt;
use std::error::Error;

use crate::utils;
use crate::debug::{DebugSymbol, SourceError};


pub type ErrorKind = CompileErrorKind;
pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub enum CompileErrorKind {
    // undefined locals, etc
    ConstPoolLimit,
}

#[derive(Debug)]
pub struct CompileError {
    kind: ErrorKind,
    symbol: Option<DebugSymbol>,
    cause: Option<Box<dyn Error>>,
}

impl CompileError {
    pub fn with_symbol(mut self, symbol: DebugSymbol) -> Self {
        self.symbol.replace(symbol); self 
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
            ErrorKind::ConstPoolLimit => "constant pool limit reached",
        };
        
        utils::format_error(fmt, "compile error", Some(message), self.source())
    }
}