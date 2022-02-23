pub use crate::lexer::TokenIndex;


// metadata attached to parser output for error handling and debug output
// will probably be attached to the statement level


#[derive(Clone, Debug)]
pub struct DebugSymbol<'a> {
    pub file: &'a str,
    pub start: TokenIndex,
    pub end: TokenIndex,
}

impl<'a> DebugSymbol<'a> {
    pub fn new(file: &'a str, start: TokenIndex, end: TokenIndex) -> Self {
        DebugSymbol { file, start, end }
    }
}

pub trait HasDebugSymbol {
    fn debug_symbol(&self) -> &DebugSymbol;
}