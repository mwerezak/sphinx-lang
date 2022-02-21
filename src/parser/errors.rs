use std::fmt;
use std::error::Error;


// Box any owned TokenMeta to prevent bloat
#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    RanOutOfTokens,
    LexerError,
    ExpectedExpr,
    ExpectedAtom,
    ExpectedGroupClose,
    ExpectedIndexingClose,
    ExpectedAccessIdentifier,  // expected an identifier following a "."
    ExpectedVarAssignment,     // expected an assignment following "var"
    InvalidAssignmentLHS,
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


