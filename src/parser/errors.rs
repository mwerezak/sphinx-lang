use std::error::Error;
use crate::lexer::{TokenMeta, Token};

pub enum ErrorKind {
    RanOutOfTokens,
    UnexpectedToken {
        found: TokenMeta,
        expected: Token,
    },
    UnexpectedTokenClass {
        found: TokenMeta,
        expected: TokenClass,
    },
}

pub struct ParserError {
    kind: ErrorKind,
}

impl ParserError {
    pub fn new(kind: ErrorKind) -> Self {
        ParserError {
            kind,
        }
    }
    
    pub fn unexpected_token(found: TokenMeta, expected: Token) -> Self {
        ParserError::new(ErrorKind::UnexpectedToken {
            expected, found
        })
    }
    
    pub fn unexpected_token_class(found: TokenMeta, expected: TokenClass) -> Self {
        ParserError::new(ErrorKind::UnexpectedTokenClass {
            expected, found
        })
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
}

pub enum TokenClass {
    Atom,
}