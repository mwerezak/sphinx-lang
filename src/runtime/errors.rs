use std::fmt;
use std::error::Error;

use crate::utils;
use crate::runtime::Variant;
use crate::runtime::strings::StringSymbol;
use crate::runtime::types::function::Signature;
use crate::debug::traceback::{TraceSite, Traceback};

// TODO box error
pub type ExecResult<T> = Result<T, RuntimeError>;

#[derive(Debug)]
pub enum ErrorKind {
    // TODO replace variant with type name
    InvalidUnaryOperand(Variant),  // unsupported operand for type
    InvalidBinaryOperand(Variant, Variant),
    OverflowError,
    NegativeShiftCount,
    NameNotDefined(String),
    CantAssignImmutable,  // can't assign to immutable global variable
    UnhashableValue(Variant),
    NotCallable(Variant),
    MissingArguments(usize, Signature),
    TooManyArguments(usize, Signature),
    AssertFailed,
    Other,
}

impl From<ErrorKind> for RuntimeError {
    fn from(kind: ErrorKind) -> Self {
        RuntimeError { kind, traceback: Vec::new(), cause: None }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    kind: ErrorKind,
    traceback: Vec<TraceSite>,
    cause: Option<Box<dyn Error>>,
}

impl RuntimeError {
    pub fn new(error: impl Error + 'static) -> Self {
        RuntimeError::from(ErrorKind::Other).caused_by(error)
    }
    
    pub fn caused_by(mut self, cause: impl Error + 'static) -> Self {
        self.cause.replace(Box::new(cause)); self
    }
    
    pub fn extend_trace(mut self, trace: impl Iterator<Item=TraceSite>) -> Self {
        self.traceback.extend(trace); self
    }
    
    pub fn push_frame(mut self, site: TraceSite) -> Self {
        self.traceback.push(site); self
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
    
    pub fn traceback(&self) -> Traceback<'_> {
        Traceback::build(self.traceback.iter())
    }
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
            // TODO
            ErrorKind::InvalidUnaryOperand(..) => format!("unsupported operand for type '...'"),
            ErrorKind::InvalidBinaryOperand(..) => format!("unsupported operand for type '...' and '...'"),
            ErrorKind::OverflowError => format!("integer arithmetic overflow"),
            ErrorKind::NegativeShiftCount => format!("negative bitshift count"),
            ErrorKind::NameNotDefined(name) => format!("undefined variable \"{}\"", name),
            ErrorKind::CantAssignImmutable => format!("can't assign to an immutable variable"),
            ErrorKind::UnhashableValue(..) => format!("unhashable value"),
            ErrorKind::NotCallable(..) => format!("'...' type is not callable"),
            ErrorKind::MissingArguments(..) => format!("name() missing N required arguments: '...', '...', and '...'"),
            ErrorKind::TooManyArguments(..) => format!("name() takes  N arguments but M were given"),
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