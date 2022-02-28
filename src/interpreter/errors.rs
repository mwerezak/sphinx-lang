use std::fmt;
use std::error::Error;


pub type EvalResult<T> = Result<T, EvalError>;

#[derive(Debug)]
pub enum EvalErrorKind {
    
}

#[derive(Debug)]
pub struct EvalError {
    kind: EvalErrorKind,
    cause: Option<Box<dyn Error>>,
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
