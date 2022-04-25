use core::fmt;
use std::error::Error;

use crate::utils;
use crate::parser::stmt::Label;
use crate::debug::{DebugSymbol, SourceError};


pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub enum ErrorKind {
    CantAssignImmutable,
    CantAssignNonLocal,
    TupleLenMismatch,
    CantUpdateAssignTuple,
    CantResolveBreak(Option<Label>),
    InvalidBreakWithValue,
    InternalLimit(&'static str),
}

#[derive(Debug)]
pub struct CompileError {
    kind: ErrorKind,
    symbol: Option<DebugSymbol>,
    cause: Option<Box<dyn Error>>,
}

impl CompileError {
    pub fn new(kind: ErrorKind) -> Self {
        Self { kind, symbol: None, cause: None }
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
        Self::new(kind)
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
            ErrorKind::CantAssignImmutable => "can't assign to immutable local variable",
            ErrorKind::CantAssignNonLocal => "can't assign to a non-local variable without the \"nonlocal\" keyword",
            ErrorKind::TupleLenMismatch => "can't assign tuples of different lengths",
            ErrorKind::CantUpdateAssignTuple => "can't use update-assigment when assigning to a tuple",
            
            ErrorKind::CantResolveBreak(label) => 
                if label.is_some() { "can't find loop or block with matching label for \"break\"" }
                else { "\"break\" outside of loop or block" },
            
            ErrorKind::InvalidBreakWithValue => "\"break\" with value outside of block expression",
            
            ErrorKind::InternalLimit(message) => message,
        };
        
        utils::format_error(fmt, "Compile error", Some(message), self.source())
    }
}