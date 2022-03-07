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

impl Atom {
    pub fn boolean(value: bool) -> Self {
        Self::BooleanLiteral(value) 
    }
    
    pub fn integer(value: language::IntType) -> Self {
        Self::IntegerLiteral(value)
    }
    
    pub fn float(value: language::FloatType) -> Self {
        Self::FloatLiteral(value) 
    }
    
    pub fn identifier(name: &str, interner: &mut StringInterner) -> Self {
        Self::Identifier(InternSymbol::from_str(name, interner))
    }
    
    pub fn string_literal(value: &str, interner: &mut StringInterner) -> Self {
        Self::StringLiteral(InternSymbol::from_str(value, interner))
    }
    
    pub fn group(expr: ExprVariant) -> Self {
        Self::Group(Box::new(expr))
    }
    
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
    
    pub fn push_access_name(&mut self, name: &str, interner: &mut StringInterner) {
        self.path.push(AccessItem::Attribute(InternSymbol::from_str(name, interner)))
    }
    
    pub fn push_access_index(&mut self, expr: Expr) {
        self.path.push(AccessItem::Index(Box::new(expr)))
    }
    
    //pub fn push_invoke(&mut self, )
    
    pub fn push_construct(&mut self, ctor: ObjectConstructor) {
        self.path.push(AccessItem::Construct(ctor))
    }
    
    pub fn pop_path(&mut self) -> Option<AccessItem> {
        self.path.pop()
    }
}

