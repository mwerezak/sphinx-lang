use crate::language::{IntType, FloatType, InternSymbol};
use crate::parser::expr::{ExprMeta, Expr, TableItem};
use crate::parser::pattern::MatchAction;


// Primary Expressions

#[derive(Debug, Clone)]
pub enum Atom {
    Nil,
    EmptyTuple,
    // Self_,
    // Super,
    
    Identifier(InternSymbol),
    BooleanLiteral(bool),
    IntegerLiteral(IntType),
    FloatLiteral(FloatType),
    StringLiteral(InternSymbol),
    
    Group {
        modifier: Option<MatchAction>,
        inner: Box<Expr>,
    }
}

// These are the highest precedence operations in the language
#[derive(Debug, Clone)]
pub enum AccessItem {
    Attribute(InternSymbol),
    Index(ExprMeta),
    Invoke(Box<[ExprMeta]>),
    InvokeTable(Box<[TableItem]>),
}

#[derive(Debug, Clone)]
pub struct Primary {
    atom: Atom,
    path: Box<[AccessItem]>,
}

impl Primary {
    pub fn new(atom: Atom, path: Vec<AccessItem>) -> Self {
        Primary { atom, path: path.into_boxed_slice() }
    }
    
    pub fn take(self) -> (Atom, Vec<AccessItem>) {
        (self.atom, self.path.into_vec())
    }
    
    pub fn atom(&self) -> &Atom { &self.atom }
    
    pub fn path(&self) -> &[AccessItem] { &self.path }
    pub fn path_mut(&mut self) -> &mut [AccessItem] { &mut self.path }
}

