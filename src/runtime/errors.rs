use std::fmt;
use std::error::Error;

// TODO box error
pub type ExecResult<T> = Result<T, RuntimeError>;

pub type ErrorKind = RuntimeErrorKind;

#[derive(Debug)]
pub enum RuntimeErrorKind {
    InvalidUnaryOperand,  // unsupported operand for type
    InvalidBinaryOperand,
    OverflowError,
    NegativeShiftCount,
    NameNotDefined(String),
    NameNotDefinedLocal(String),
    CantAssignImmutable,
    CantAssignNonLocal,  // can't assign to a non-local variable without the "global" keyword
    UnhashableType,
    Other,
}

impl From<RuntimeErrorKind> for RuntimeError {
    fn from(kind: RuntimeErrorKind) -> Self {
        RuntimeError { kind, cause: None }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    kind: RuntimeErrorKind,
    cause: Option<Box<dyn Error>>,
}

impl RuntimeError {
    pub fn new(error: impl Error + 'static) -> Self {
        RuntimeError::from(RuntimeErrorKind::Other).caused_by(error)
    }
    
    pub fn caused_by(mut self, cause: impl Error + 'static) -> Self {
        self.cause = Some(Box::new(cause)); self
    }
    
    pub fn kind(&self) -> &RuntimeErrorKind { &self.kind }
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