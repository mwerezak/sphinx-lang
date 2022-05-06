use core::fmt;
use std::error::Error;

use crate::utils;
use crate::debug::{DebugSymbol, SourceError};


pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub struct CompileError {
    message: String,
    symbol: Option<DebugSymbol>,
    cause: Option<Box<dyn Error>>,
}

impl CompileError {
    pub fn new(message: &str) -> Self {
        Self { message: message.to_string(), symbol: None, cause: None }
    }
    
    pub fn with_symbol(mut self, symbol: DebugSymbol) -> Self {
        self.symbol.get_or_insert(symbol); self 
    }
    
    pub fn caused_by(mut self, error: impl Error + 'static) -> Self {
        self.cause.replace(Box::new(error)); self
    }
}

impl<S> From<S> for CompileError where S: AsRef<str> {
    fn from(message: S) -> Self {
        Self::new(message.as_ref())
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
        let message =
            if self.message.is_empty() { None }
            else { Some(self.message.as_str()) };
        
        utils::format_error(fmt, "Compile error", message, self.source())
    }
}