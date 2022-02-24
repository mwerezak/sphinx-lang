pub use crate::lexer::TokenIndex;


// metadata attached to parser output for error handling and debug output
// will probably be attached to the statement level


#[derive(Debug, Clone)]
pub struct DebugSymbol<'m> {
    pub module: &'m str,  // TODO reference module instead of just str
    pub start: TokenIndex,
    pub end: TokenIndex,
}

impl<'m> DebugSymbol<'m> {
    pub fn new(module: &'m str, start: TokenIndex, end: TokenIndex) -> Self {
        DebugSymbol { module, start, end }
    }
}

pub trait HasDebugSymbol {
    fn debug_symbol(&self) -> &DebugSymbol;
}