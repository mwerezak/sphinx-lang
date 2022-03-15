use std::fmt;
use std::error::Error;

use crate::runtime::Variant;

// TODO box error
pub type ExecResult<T> = Result<T, RuntimeError>;

#[derive(Debug)]
pub enum ErrorKind {
    InvalidUnaryOperand(Variant),  // unsupported operand for type
    InvalidBinaryOperand(Variant, Variant),
    OverflowError,
    NegativeShiftCount,
    NameNotDefined(String),
    NameNotDefinedLocal(String),
    CantAssignImmutable,
    CantAssignNonLocal,  // can't assign to a non-local variable without the "global" keyword
    UnhashableType(Variant),
    Other,
}

impl From<ErrorKind> for RuntimeError {
    fn from(kind: ErrorKind) -> Self {
        RuntimeError { kind, cause: None }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    kind: ErrorKind,
    cause: Option<Box<dyn Error>>,
}

impl RuntimeError {
    pub fn new(error: impl Error + 'static) -> Self {
        RuntimeError::from(ErrorKind::Other).caused_by(error)
    }
    
    pub fn caused_by(mut self, cause: impl Error + 'static) -> Self {
        self.cause = Some(Box::new(cause)); self
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
}

impl Error for RuntimeError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, _fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}

/*
Probably declare these in the debug module...

pub struct Frame {
    symbol: DebugSymbol,
    context: ...,
}

pub struct Traceback {
    frames: Vec<Frame>,
}
*/