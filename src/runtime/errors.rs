use std::fmt;
use std::error::Error;

#[derive(Debug)]
pub enum ErrorKind {
    UnsupportedUnaryOperand,
    UnsupportedBinaryOperand,  // "unsupported operand types: 'a' and 'b'
}

#[derive(Debug)]
pub struct RuntimeError {
    kind: ErrorKind,
}

impl Error for RuntimeError {
    
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, _fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}