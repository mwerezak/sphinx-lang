use std::fmt;
use crate::lexer::Span;

// Lexer Errors

// TODO error type enum
#[derive(Debug)]
pub struct LexerError {
    pub etype: LexerErrorType,
    pub location: Span,
    pub lineno: u64,
}

impl LexerError {
    pub fn new(etype: LexerErrorType, location: Span, lineno: u64) -> Self {
        LexerError { etype, location, lineno }
    }
}


#[derive(Debug)]
pub enum LexerErrorType {
    NoMatchingRule,
    AmbiguousMatch,
    UnexpectedEOF,
}

impl fmt::Display for LexerErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LexerErrorType::NoMatchingRule => write!(f, "invalid token"),
            LexerErrorType::AmbiguousMatch => write!(f, "ambiguous token"),
            LexerErrorType::UnexpectedEOF  => write!(f, "unexpected EOF"),
        }
    }
}