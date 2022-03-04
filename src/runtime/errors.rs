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

// Runtime errors are so wide in scope, this is just a smart pointer to an error trait object

pub type ExecResult<T> = Result<T, RuntimeError>;

#[derive(Debug)]
pub struct RuntimeError {
    error: Box<dyn Error>,
}

impl<E> From<E> for RuntimeError where E: Error + 'static {
    fn from(error: E) -> Self {
        RuntimeError { error: Box::new(error) }
    }
}

impl std::ops::Deref for RuntimeError {
    type Target = dyn Error;
    fn deref(&self) -> &Self::Target {
        &*self.error
    }
}

impl RuntimeError {
    pub fn downcast<E>(self) -> Result<Box<E>, Box<(dyn Error + 'static)>> where E: Error + 'static {
        self.error.downcast::<E>()
    }
}