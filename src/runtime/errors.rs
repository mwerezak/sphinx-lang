use core::fmt;
use std::error::Error;

use crate::utils;
use crate::runtime::gc::GcTrace;
use crate::runtime::strings::StringValue;
use crate::debug::traceback::{TraceSite, Traceback};

mod errorkinds;
pub use errorkinds::ErrorKind;


pub type ExecResult<T> = Result<T, Box<RuntimeError>>;


#[derive(Debug, Clone)]
pub struct RuntimeError {
    kind: ErrorKind,
    message: StringValue,
    traceback: Vec<TraceSite>,
    cause: Option<Box<RuntimeError>>,
}

unsafe impl GcTrace for RuntimeError {
    fn trace(&self) {
        self.message.trace();
        
        for site in self.traceback.iter() {
            site.trace();
        }
        
        if let Some(error) = self.cause.as_ref() {
            error.trace();
        }
    }
}

impl RuntimeError {
    pub fn new(kind: ErrorKind, message: StringValue) -> Self {
        Self {
            kind, message,
            traceback: Vec::new(),
            cause: None,
        }
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

// traceback construction
impl RuntimeError {
    pub fn caused_by(mut self: Box<Self>, cause: Box<RuntimeError>) -> Box<Self> {
        self.cause.replace(cause); self
    }
    
    pub fn extend_trace(mut self: Box<Self>, trace: impl Iterator<Item=TraceSite>) -> Box<Self> {
        self.traceback.extend(trace); self
    }
    
    pub fn push_trace(mut self: Box<Self>, site: TraceSite) -> Box<Self> {
        self.traceback.push(site); self
    }
}

#[allow(clippy::useless_format)]
impl fmt::Display for RuntimeError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let message = format!("{}", self.message);
        utils::format_error(fmt, "Runtime error", Some(&message), self.source())
    }
}
