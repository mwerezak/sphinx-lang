use crate::language;
use crate::runtime::strings::{InternSymbol, StringInterner};
use crate::parser::expr::{Expr, ExprVariant};
use crate::parser::structs::ObjectConstructor;


// Primary Expressions

#[derive(Debug, Clone)]
pub enum Atom {
    Nil,
    EmptyTuple,
    Self_,
    Super,
    Identifier(InternSymbol),
    BooleanLiteral(bool),
    IntegerLiteral(language::IntType),
    FloatLiteral(language::FloatType),
    StringLiteral(InternSymbol),
    Group(Box<ExprVariant>), // type annotation
}

// These are the highest precedence operations in the language
#[derive(Debug, Clone)]
pub enum AccessItem {
    Attribute(InternSymbol),
    Index(Box<Expr>),
    Invoke(),       // TODO
    Construct(ObjectConstructor),
}

#[derive(Debug, Clone)]
pub struct Primary {
    atom: Atom,
    path: Vec<AccessItem>,
}

impl Primary {
    pub fn new(atom: Atom) -> Self {
        Primary { atom, path: Vec::new() }
    }
    
    pub fn with_path<I>(atom: Atom, path: impl Iterator<Item=AccessItem>) -> Self {
        Primary { atom, path: path.collect() }
    }
    
    pub fn atom(&self) -> &Atom { &self.atom }
    pub fn take_atom(self) -> Atom { self.atom }
    
    pub fn has_path(&self) -> bool { !self.path.is_empty() }
    pub fn path_len(&self) -> usize { self.path.len() }
    
    pub fn iter_path(&self) -> impl Iterator<Item=&AccessItem> {
        self.path.iter()
    }
    
    pub fn push_access_attr(&mut self, name: InternSymbol) {
        self.path.push(AccessItem::Attribute(name))
    }
    
    pub fn push_access_index(&mut self, expr: Expr) {
        self.path.push(AccessItem::Index(Box::new(expr)))
    }
    
    //pub fn push_invoke(&mut self, )
    
    pub fn push_construct(&mut self, ctor: ObjectConstructor) {
        self.path.push(AccessItem::Construct(ctor))
    }
    
    pub fn push_path(&mut self, item: AccessItem) {
        self.path.push(item);
    }
    
    pub fn pop_path(&mut self) -> Option<AccessItem> {
        self.path.pop()
    }
}

