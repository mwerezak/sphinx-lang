use core::fmt;
use std::rc::Rc;
use std::error::Error;

use crate::utils;
use crate::debug::symbol::DebugSymbol;


#[derive(Debug)]
pub enum ErrorKind {
    EOFReached, // hit EOF before reaching the indicated end of symbol
    Other,
}

#[derive(Debug)]
pub struct SymbolResolutionError {
    kind: ErrorKind,
    symbol: DebugSymbol,
    cause: Option<Rc<dyn Error>>,
}

impl SymbolResolutionError {
    pub fn new(symbol: DebugSymbol, kind: ErrorKind) -> Self {
        Self {
            symbol, kind, cause: None,
        }
    }
    
    pub fn caused_by(symbol: DebugSymbol, error: Rc<dyn Error>) -> Self {
        Self {
            symbol,
            kind: ErrorKind::Other,
            cause: Some(error),
        }
    }
    
    pub fn kind(&self) -> &ErrorKind { &self.kind }
}

impl Error for SymbolResolutionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.cause.as_ref().map(|o| o.as_ref())
    }
}

impl fmt::Display for SymbolResolutionError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        
        let inner_message = match self.kind() {
            ErrorKind::EOFReached => Some("EOF reached while extracting symbol"),
            ErrorKind::Other => None,
        };
        
        let message = format!("could not resolve symbol ${}:{}", self.symbol.start, self.symbol.end());
        utils::format_error(fmt, message.as_str(), inner_message, self.source())
    }
}
