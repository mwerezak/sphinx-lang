use std::fmt;
use std::error::Error;

use crate::utils;
use crate::runtime::Variant;
use crate::runtime::function::Signature;
use crate::runtime::gc::{GC, GCTrace};
use crate::debug::traceback::{TraceSite, Traceback};

// TODO box error
pub type ExecResult<T> = Result<T, Box<RuntimeError>>;

#[derive(Debug)]
pub enum ErrorKind {
    // TODO replace variant with type name
    InvalidUnaryOperand(Variant),  // unsupported operand for type
    InvalidBinaryOperand(Variant, Variant),
    OverflowError,
    DivideByZero,
    NegativeShiftCount,
    NameNotDefined(String),
    CantAssignImmutable,  // can't assign to immutable global variable
    UnhashableValue(Variant),
    NotCallable(Variant),
    MissingArguments { callable: GC<Signature>, nargs: usize },
    TooManyArguments { callable: GC<Signature>, nargs: usize },
    AssertFailed,
}

impl From<ErrorKind> for RuntimeError {
    fn from(kind: ErrorKind) -> Self {
        RuntimeError { kind, traceback: Vec::new(), cause: None }
    }
}

impl From<ErrorKind> for Box<RuntimeError> {
    fn from(kind: ErrorKind) -> Self {
        Box::new(kind.into())
    }
}

unsafe impl GCTrace for ErrorKind {
    fn trace(&self) {
        match self {
            Self::InvalidUnaryOperand(op) => op.trace(),
            Self::InvalidBinaryOperand(lhs, rhs) => { lhs.trace(); rhs.trace() },
            Self::UnhashableValue(value) => value.trace(),
            Self::NotCallable(value) => value.trace(),
            Self::MissingArguments { callable, .. } => callable.mark_trace(),
            Self::TooManyArguments { callable, .. } => callable.mark_trace(),
            _ => { },
        }
    }
}



#[derive(Debug)]
pub struct RuntimeError {
    kind: ErrorKind,
    traceback: Vec<TraceSite>,
    cause: Option<Box<RuntimeError>>,
}

unsafe impl GCTrace for RuntimeError {
    fn trace(&self) {
        self.kind.trace();
        for site in self.traceback.iter() {
            site.trace();
        }
    }
}

impl RuntimeError {
    pub fn caused_by(mut self: Box<Self>, cause: Box<RuntimeError>) -> Box<Self> {
        self.cause.replace(cause); self
    }
    
    pub fn extend_trace(mut self: Box<Self>, trace: impl Iterator<Item=TraceSite>) -> Box<Self> {
        self.traceback.extend(trace); self
    }
    
    pub fn push_frame(mut self: Box<Self>, site: TraceSite) -> Box<Self> {
        self.traceback.push(site); self
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
    
    pub fn traceback(&self) -> Traceback<'_> {
        Traceback::build(self.traceback.iter())
    }
}

impl Error for RuntimeError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(
            |error| &*error as &RuntimeError as &dyn Error
        )
    }
}

#[allow(clippy::useless_format)]
impl fmt::Display for RuntimeError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let message = match self.kind() {
            // TODO
            ErrorKind::InvalidUnaryOperand(..) => format!("unsupported operand for type '...'"),
            ErrorKind::InvalidBinaryOperand(..) => format!("unsupported operand for type '...' and '...'"),
            ErrorKind::DivideByZero => format!("divide by zero"),
            ErrorKind::OverflowError => format!("integer arithmetic overflow"),
            ErrorKind::NegativeShiftCount => format!("negative bitshift count"),
            ErrorKind::NameNotDefined(name) => format!("undefined variable \"{}\"", name),
            ErrorKind::CantAssignImmutable => format!("can't assign to an immutable variable"),
            ErrorKind::UnhashableValue(..) => format!("unhashable value"),
            ErrorKind::NotCallable(..) => format!("'...' type is not callable"),
            ErrorKind::MissingArguments { .. } => format!("<callable> missing N required arguments: '...', '...', and '...'"),
            ErrorKind::TooManyArguments { .. } => format!("<callable> takes N arguments but M were given"),
            ErrorKind::AssertFailed => format!("assertion failed"),
        };
        
        utils::format_error(fmt, "Runtime error", Some(&message), self.source())
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