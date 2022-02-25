use std::fmt;
use std::error::Error;
use crate::runtime::types::TypeID;

pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Debug)]
pub enum ErrorKind {
    UnsupportedUnaryOperand(TypeID),   // "unsupported operand type: 'a'
    UnsupportedBinaryOperand(TypeID, TypeID),  // "unsupported operand types: 'a' and 'b'
    TypeIDAlreadyTaken(TypeID),
}

#[derive(Debug)]
pub struct RuntimeError {
    kind: ErrorKind,
}

impl RuntimeError {
    pub fn new(kind: ErrorKind) -> Self {
        RuntimeError {
            kind,
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