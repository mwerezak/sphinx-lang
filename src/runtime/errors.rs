use std::fmt;
use std::error::Error;

use crate::utils;
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
    CantAssignImmutable,  // can't assign to immutable global variable
    UnhashableValue(Variant),
    AssertFailed,
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

#[allow(clippy::useless_format)]
impl fmt::Display for RuntimeError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let message = match self.kind() {
            ErrorKind::InvalidUnaryOperand(..) => format!("unsupported operand for type '...'"),
            ErrorKind::InvalidBinaryOperand(..) => format!("unsupported operand for type '...' and '...'"),
            ErrorKind::OverflowError => format!("integer arithmetic overflow"),
            ErrorKind::NegativeShiftCount => format!("negative bitshift count"),
            ErrorKind::NameNotDefined(name) => format!("undefined variable \"{}\"", name),
            ErrorKind::CantAssignImmutable => format!("can't assign to an immutable variable"),
            ErrorKind::UnhashableValue(..) => format!("unhashable value"),
            ErrorKind::AssertFailed => format!("assertion failed"),
            ErrorKind::Other => String::new(),
        };
        
        utils::format_error(fmt, "runtime error", Some(message.as_str()), self.source())
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