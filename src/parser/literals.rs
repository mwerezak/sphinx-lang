use std::fmt;
use crate::parser::ast::AstNode;

#[derive(Debug)]
pub struct Identifier {
    ident: String,
}

impl Identifier {
    pub fn new<S: ToString>(s: S) -> Self {
        Identifier { ident: s.to_string() }
    }
}

impl AstNode for Identifier { }

impl fmt::Display for Identifier {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self.ident.as_str())
    }
}