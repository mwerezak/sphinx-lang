use std::fmt;
use crate::language;
use crate::runtime::data::{InternStr, StringInterner};
use crate::parser::expr::{Expr, ExprVariant};
use crate::parser::structs::ObjectConstructor;


// Primary Expressions

#[derive(Debug, Clone)]
pub enum Atom {
    Nil,
    EmptyTuple,
    Self_,
    Super,
    Identifier(InternStr),
    UpvalIdentifier(InternStr),
    GlobalIdentifier(InternStr),
    BooleanLiteral(bool),
    IntegerLiteral(language::IntType),
    FloatLiteral(language::FloatType),
    StringLiteral(InternStr),
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
        Self::Identifier(InternStr::from_str(name, interner))
    }
    
    pub fn global_identifier(name: &str, interner: &mut StringInterner) -> Self {
        Self::GlobalIdentifier(InternStr::from_str(name, interner))
    }
    
    pub fn upval_identifier(name: &str, interner: &mut StringInterner) -> Self {
        Self::UpvalIdentifier(InternStr::from_str(name, interner))
    }
    
    pub fn string_literal(value: &str, interner: &mut StringInterner) -> Self {
        Self::StringLiteral(InternStr::from_str(value, interner))
    }
    
    pub fn group(expr: ExprVariant) -> Self {
        Self::Group(Box::new(expr))
    }
    
}

// These are the highest precedence operations in the language
#[derive(Debug, Clone)]
pub enum AccessItem {
    Access(InternStr),
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
    
    pub fn iter_path(&self) -> impl Iterator<Item=&AccessItem> {
        self.path.iter()
    }
    
    /*
        Check if this primary expression is also an lvalue.
        
        lvalue ::= IDENTIFIER | "global" IDENTIFIER | primary subscript | primary access ;
    */
    pub fn is_lvalue(&self) -> bool {
        if self.path.is_empty() {
            matches!(self.atom, Atom::Identifier(..) | Atom::GlobalIdentifier(..))
        } else {
            let last_op = self.path.last().unwrap();
            matches!(last_op, AccessItem::Access(..) | AccessItem::Index(..))
        }
    }
    
    pub fn push_access_member(&mut self, name: &str, interner: &mut StringInterner) {
        self.path.push(AccessItem::Access(InternStr::from_str(name, interner)))
    }
    
    pub fn push_access_index(&mut self, expr: Expr) {
        self.path.push(AccessItem::Index(Box::new(expr)))
    }
    
    //pub fn push_invoke(&mut self, )
    
    pub fn push_construct(&mut self, ctor: ObjectConstructor) {
        self.path.push(AccessItem::Construct(ctor))
    }
}

