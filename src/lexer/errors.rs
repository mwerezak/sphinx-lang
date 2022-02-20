use std::error::Error;
use crate::lexer::Span;


// Lexer Errors

#[derive(Debug)]
pub enum LexerErrorKind {
    NoMatchingRule,
    UnexpectedEOF,
    CouldNotReadToken(Box<dyn Error>),  // kludge
}

#[derive(Debug)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub location: Span,
}


// invalid token"),
// "unexpected EOF")