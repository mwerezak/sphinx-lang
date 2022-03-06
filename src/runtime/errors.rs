use std::fmt;
use std::error::Error;
use crate::utils;

pub type EvalResult<T> = Result<T, EvalError>;

#[derive(Debug)]
pub enum EvalErrorKind {
    InvalidUnaryOperand,  // unsupported operand for type
    InvalidBinaryOperand,
    OverflowError,
    NegativeShiftCount,
}

impl From<EvalErrorKind> for EvalError {
    fn from(kind: EvalErrorKind) -> Self {
        EvalError { kind, cause: None }
    }
}

#[derive(Debug)]
pub struct EvalError {
    kind: EvalErrorKind,
    cause: Option<Box<dyn Error>>,
}

impl EvalError {
    pub fn caused_by(mut self, cause: impl Error + 'static) -> Self {
        self.cause = Some(Box::new(cause)); self
    }
}

impl Error for EvalError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, _fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}


use std::any::Any;

pub type ExecResult<T> = Result<T, RuntimeError>;

#[derive(Debug)]
pub enum RuntimeErrorKind {
    EvalError,
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

impl From<EvalError> for RuntimeError {
    fn from(error: EvalError) -> Self {
        RuntimeError::from(RuntimeErrorKind::EvalError).caused_by(error)
    }
}