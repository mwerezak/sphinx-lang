pub use crate::lexer::TokenIndex;

use std::path::Path;


// metadata attached to parser output for error handling and debug output
// will probably be attached to the statement level


#[derive(Debug, Clone)]
pub struct DebugSymbol {
    pub start: TokenIndex,
    pub end: TokenIndex,
}

impl DebugSymbol {
    pub fn new(start: TokenIndex, end: TokenIndex) -> Self {
        DebugSymbol { start, end }
    }
}


// Temporary, hopefully.
// Should eventually provide the information necessary 
// to reload the source code text for error reporting
#[derive(Debug)]
pub struct ModuleSource {
    name: String,
    path: Option<String>,
}

impl ModuleSource {
    pub fn new<S: ToString>(name: S, path: Option<S>) -> Self {
        ModuleSource {
            name: name.to_string(),
            path: path.map(|s| s.to_string()),
        }
    }
}