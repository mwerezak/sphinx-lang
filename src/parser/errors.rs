use std::fmt;
use std::error::Error;
use crate::lexer::TokenMeta;

#[derive(Clone, Copy, Debug)]
pub enum Expect {
    ParseAtom,
    ParseGroupCloseParen,
    ParseAccessIdentifier,
    ParseIndexingCloseSquare,
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    RanOutOfTokens,
    LexerError,
    UnexpectedToken {
        found: Box<TokenMeta>,
        expected: Expect,
    },
}

#[derive(Debug)]
pub struct ParserError {
    kind: ErrorKind,
    cause: Option<Box<dyn Error>>,
}

impl ParserError {
    pub fn new(kind: ErrorKind) -> Self {
        ParserError {
            kind, cause: None,
        }
    }
    
    pub fn caused_by(error: Box<dyn Error>, kind: ErrorKind) -> Self {
        ParserError {
            kind, cause: Some(error),
        }
    }
    
    pub fn unexpected_token(found: TokenMeta, expected: Expect) -> Self {
        ParserError::new(ErrorKind::UnexpectedToken {
            expected,
            found: Box::new(found),
        })
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
}

impl Error for ParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, _fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}

