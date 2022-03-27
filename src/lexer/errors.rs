use std::fmt;
use std::error::Error;
use crate::debug::DebugSymbol;


// Lexer Errors

pub type ErrorKind = LexerErrorKind;

#[derive(Debug)]
pub enum LexerErrorKind {
    IOError,
    UnexpectedEOF,
    NoMatchingRule,
    CouldNotReadToken,
    MaxTokenLengthExceeded,
    SourceTooLong,
}

impl fmt::Display for LexerErrorKind {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match self {
            Self::IOError => "error reading source text",
            Self::NoMatchingRule => "unrecognized token",
            Self::UnexpectedEOF => "unexpected end of file",
            Self::CouldNotReadToken => "invalid token",
            Self::MaxTokenLengthExceeded => "max token length exceeded",
            Self::SourceTooLong => "max source length exceeded",
        };
        fmt.write_str(msg)
    }
}


#[derive(Debug)]
pub struct LexerError {
    kind: ErrorKind,
    symbol: DebugSymbol,
    cause: Option<Box<dyn Error>>,
}

impl LexerError {
    pub fn new(kind: ErrorKind, symbol: DebugSymbol) -> Self {
        LexerError {
            kind, symbol,
            cause: None,
        }
    }
    
    pub fn caused_by(mut self, cause: Box<dyn Error>) -> Self {
        self.cause = Some(cause); self
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
    pub fn debug_symbol(&self) -> &DebugSymbol { &self.symbol }
    
}

impl Error for LexerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.kind)?;
        if let Some(err) = self.source() {
            write!(fmt, ": {}", err)?;
        }
        Ok(())
    }
}

