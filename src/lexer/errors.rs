use std::num::ParseIntError;
use crate::lexer::Span;

// Lexer Errors

#[derive(Debug)]
pub enum ErrorType {
    NoMatchingRule,
    UnexpectedEOF,
    ParseIntError(ParseIntError),
}

#[derive(Debug)]
pub struct LexerError {
    pub etype: ErrorType,
    pub location: Span,
    pub lineno: u64,
}

pub struct TokenError {
    pub etype: ErrorType,
}