pub use crate::lexer::TokenIndex;


// metadata attached to parser output for error handling and debug output
// will probably be attached to the statement level


#[derive(Debug, Clone)]
pub struct DebugSymbol<'m> {
    pub module: &'m ModuleSource,
    pub start: TokenIndex,
    pub end: TokenIndex,
}

impl<'m> DebugSymbol<'m> {
    pub fn new(module: &'m ModuleSource, start: TokenIndex, end: TokenIndex) -> Self {
        DebugSymbol { module, start, end }
    }
}

pub trait HasDebugSymbol {
    fn debug_symbol(&self) -> &DebugSymbol;
}


// Temporary, hopefully.
// Should eventually provide the information necessary 
// to reload the source code text for error reporting
#[derive(Debug)]
pub struct ModuleSource {
    name: String,
}