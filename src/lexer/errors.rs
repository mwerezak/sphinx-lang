use std::fmt;
use std::error::Error;
use crate::lexer::Span;


// Lexer Errors

#[derive(Debug)]
pub enum ErrorKind {
    NoMatchingRule,
    UnexpectedEOF,
    CouldNotReadToken,
}

#[derive(Debug)]
pub struct LexerError {
    pub kind: ErrorKind,
    pub location: Span,
    cause: Option<Box<dyn Error>>,
}

impl LexerError {
    pub fn new(kind: ErrorKind, location: Span) -> Self {
        LexerError {
            kind, location,
            cause: None,
        }
    }
    
    pub fn caused_by(cause: Box<dyn Error>, kind: ErrorKind, location: Span) -> Self {
        LexerError {
            kind, location,
            cause: Some(cause),
        }
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
    pub fn location(&self) -> &Span { &self.location }
    
}

impl Error for LexerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, _fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}


// invalid token"),
// "unexpected EOF")