use std::fmt;
use std::error::Error;
use crate::parser::operator::{UnaryOp, BinaryOp};
use crate::runtime::types::TypeID;


pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Debug)]
pub enum ErrorKind {
    DivideByZero,  // attempt to divide by zero
    UnsupportedUnaryOperand(UnaryOp, TypeID),   // "unsupported operand type for unary -: 'a'
    UnsupportedBinaryOperand(BinaryOp, TypeID, TypeID),  // "unsupported operand types for +: 'a' and 'b'
    TypeIDAlreadyTaken(TypeID),
}

#[derive(Debug)]
pub struct RuntimeError {
    kind: ErrorKind,
    message: Option<&'static str>,
}

impl RuntimeError {
    pub fn new(kind: ErrorKind) -> Self {
        RuntimeError {
            kind,
            message: None,
        }
    }
    
    pub fn with_message(kind: ErrorKind, message: &'static str) -> Self {
        RuntimeError {
            kind,
            message: Some(message),
        }
    }
}

impl Error for RuntimeError {
    
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, _fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}