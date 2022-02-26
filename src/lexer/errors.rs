use std::fmt;
use std::error::Error;
use crate::lexer::Span;


// Lexer Errors

#[derive(Debug)]
pub enum ErrorKind {
    IOError,
    UnexpectedEOF,
    NoMatchingRule,
    CouldNotReadToken,
    MaxTokenLengthExceeded,
    SourceTooLong,
}

impl ErrorKind {
    pub fn message(&self) -> &'static str {
        match self {
            Self::IOError => "error reading source text",
            Self::NoMatchingRule => "unrecognized token",
            Self::UnexpectedEOF => "unexpected end of file",
            Self::CouldNotReadToken => "invalid token",
            Self::MaxTokenLengthExceeded => "max token length exceeded",
            Self::SourceTooLong => "max source length exceeded",
        }
    }
}

#[derive(Debug)]
pub struct LexerError {
    pub kind: ErrorKind,
    pub span: Span,
    cause: Option<Box<dyn Error>>,
}

impl LexerError {
    pub fn new(kind: ErrorKind, span: Span) -> Self {
        LexerError {
            kind, span,
            cause: None,
        }
    }
    
    pub fn caused_by(cause: Box<dyn Error>, kind: ErrorKind, span: Span) -> Self {
        LexerError {
            kind, span,
            cause: Some(cause),
        }
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
    pub fn span(&self) -> &Span { &self.span }
    
}

impl Error for LexerError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt.write_str(self.kind.message())?;
        if let Some(err) = self.source() {
            write!(fmt, ": {}", err)?;
        }
        Ok(())
    }
}

