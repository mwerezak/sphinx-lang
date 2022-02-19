// Primary expressions

use std::fmt;
use crate::language;
use crate::parser::expr::Expr;


// Primary Expressions

#[derive(Debug)]
pub enum Atom {
    Identifier(Identifier),
    IntLiteral(language::IntType),
    FloatLiteral(language::FloatType),
    Group(Box<Expr>)
}

// These are the highest precedence operations in the language
#[derive(Debug)]
pub enum PrimaryOp {
    Access(Identifier),
    Index(Box<Expr>),
    Invoke,  // TODO
}

#[derive(Debug)]
pub struct Primary {
    atom: Atom,
    ops: Vec<PrimaryOp>,
}

impl Primary {
    pub fn new(atom: Atom) -> Self {
        Primary { atom, ops: Vec::new() }
    }
    
    pub fn with_ops<I>(atom: Atom, ops: I) -> Self
    where I: Iterator<Item=PrimaryOp> {
        Primary { atom, ops: ops.collect() }
    }
}


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


pub struct StrLiteral { }  // TODO