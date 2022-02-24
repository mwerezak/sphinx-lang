use std::fmt;
use std::error::Error;



#[derive(Clone, Debug)]
pub struct RuntimeError {
    
}

impl Error for RuntimeError {
    
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, _fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}