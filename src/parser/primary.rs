use crate::language;
use crate::runtime::strings::InternSymbol;
use crate::parser::expr::{ExprMeta, Expr};


// Primary Expressions

#[derive(Debug, Clone)]
pub enum Atom {
    Nil,
    EmptyTuple,
    // Self_,
    // Super,
    Identifier(InternSymbol),
    BooleanLiteral(bool),
    IntegerLiteral(language::IntType),
    FloatLiteral(language::FloatType),
    StringLiteral(InternSymbol),
    Group(Box<Expr>), // type annotation
}

// These are the highest precedence operations in the language
#[derive(Debug, Clone)]
pub enum AccessItem {
    Attribute(InternSymbol),
    Index(ExprMeta),
    Invoke(),       // TODO
    // Construct(ObjectConstructor),
}

#[derive(Debug, Clone)]
pub struct Primary {
    atom: Atom,
    path: Vec<AccessItem>,
}

impl Primary {
    pub fn new(atom: Atom, path: Vec<AccessItem>) -> Self {
        debug_assert!(!path.is_empty());
        Primary { atom, path }
    }
    
    pub fn atom(&self) -> &Atom { &self.atom }
    pub fn take_atom(self) -> Atom { self.atom }
    
    pub fn path(&self) -> &Vec<AccessItem> { &self.path }
    pub fn path_mut(&mut self) -> &mut Vec<AccessItem> { &mut self.path }
}

