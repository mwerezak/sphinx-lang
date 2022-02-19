use std::fmt;
use crate::language;


// Identifiers


// TODO intern all identifiers and string literals and make this Copy
#[derive(Debug, Clone)]
pub struct Identifier {
    ident: String,
}

impl Identifier {
    pub fn new<S: ToString>(s: S) -> Self {
        Identifier { ident: s.to_string() }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self.ident.as_str())
    }
}


// Integer Literals

#[derive(Debug, Clone, Copy)]
pub struct IntLiteral {
    value: language::IntType,
}

impl IntLiteral {
    pub fn new(value: language::IntType) -> Self {
        IntLiteral { value }
    }
}

impl fmt::Display for IntLiteral {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(fmt)
    }
}