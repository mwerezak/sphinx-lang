use std::error::Error;
use crate::lexer::Span;


// Lexer Errors

#[derive(Debug)]
pub enum ErrorKind {
    NoMatchingRule,
    UnexpectedEOF,
    CouldNotReadToken(Box<dyn Error>),  // kludge
}

#[derive(Debug)]
pub struct LexerError {
    pub kind: ErrorKind,
    pub location: Span,
}


// invalid token"),
// "unexpected EOF")