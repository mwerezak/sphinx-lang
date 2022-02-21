// Primary expressions

use std::fmt;
use crate::language;
use crate::parser::expr::Expr;


// Primary Expressions

#[derive(Debug, Clone)]
pub enum Atom {
    Nil,
    Identifier(Name),
    BooleanLiteral(bool),
    IntegerLiteral(language::IntType),
    FloatLiteral(language::FloatType),
    Group(Box<Expr>)
}

impl Atom {
    // pub fn bool(value: bool) -> Self { Self::BooleanLiteral(value) }
    // pub fn int(value: language::IntType) -> Self { Self::IntegerLiteral(value) }
    // pub fn float(value: language::FloatType) -> Self { Self::FloatLiteral(value) }
    
    pub fn identifier(name: &str) -> Self {
        Self::Identifier(Name::new(name))
    }
    
    pub fn group(expr: Expr) -> Self {
        Self::Group(Box::new(expr))
    }
}

// These are the highest precedence operations in the language
#[derive(Debug, Clone)]
pub enum PrimaryOp {
    Access(Name),
    Index(Box<Expr>),
    Invoke,  // TODO
}

#[derive(Debug, Clone)]
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
    
    /*
        Check if this primary expression is also an lvalue.
        
        lvalue ::= IDENTIFIER | primary subscript | primary access ;
    */
    pub fn is_lvalue(&self) -> bool {
        // if there are no primary ops, then we are in the IDENTIFIER branch
        if self.ops.is_empty() {
            matches!(self.atom, Atom::Identifier(..))
        } else {
            let last_op = self.ops.last().unwrap();
            
            matches!(last_op, PrimaryOp::Access(..) | PrimaryOp::Index(..))
        }
    }
    
    pub fn push_access(&mut self, name: &str) {
        self.ops.push(PrimaryOp::Access(Name::new(name)))
    }
    
    pub fn push_indexing(&mut self, expr: Expr) {
        self.ops.push(PrimaryOp::Index(Box::new(expr)))
    }
}


// Identifiers

// TODO intern all identifier names and string literals and make this Copy
#[derive(Debug, Clone)]
pub struct Name {
    name: String,
}

impl Name {
    pub fn new(name: &str) -> Self {
        Name { name: name.to_string() }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self.name.as_str())
    }
}


pub struct StrLiteral { }  // TODO