use std::fmt;
use std::error::Error;


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
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}

