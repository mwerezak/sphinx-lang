use std::fmt;
use string_interner::{Symbol, DefaultSymbol, StringInterner};
use string_interner::backend::Backend;

use crate::language;
use crate::parser::expr::Expr;
use crate::parser::structs::ObjectConstructor;


// Primary Expressions

#[derive(Debug, Clone)]
pub enum Atom {
    Nil,
    EmptyTuple,
    Identifier(InternStr),
    BooleanLiteral(bool),
    IntegerLiteral(language::IntType),
    FloatLiteral(language::FloatType),
    StringLiteral(InternStr),
    Group(Box<Expr>),
}

impl Atom {
    // pub fn bool(value: bool) -> Self { Self::BooleanLiteral(value) }
    // pub fn int(value: language::IntType) -> Self { Self::IntegerLiteral(value) }
    // pub fn float(value: language::FloatType) -> Self { Self::FloatLiteral(value) }
    
    pub fn identifier(name: &str, interner: &mut StringInterner) -> Self {
        Self::Identifier(InternStr::from_str(name, interner))
    }
    
    pub fn string_literal(value: &str, interner: &mut StringInterner) -> Self {
        Self::StringLiteral(InternStr::from_str(value, interner))
    }
    
    pub fn group(expr: Expr) -> Self {
        Self::Group(Box::new(expr))
    }
    
}

// These are the highest precedence operations in the language
#[derive(Debug, Clone)]
pub enum AccessItem {
    Member(InternStr),
    Index(Box<Expr>),
    Invoke,       // TODO
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
    
    pub fn with_ops<I>(atom: Atom, path: I) -> Self
    where I: Iterator<Item=AccessItem> {
        Primary { atom, path: path.collect() }
    }
    
    /*
        Check if this primary expression is also an lvalue.
        
        lvalue ::= IDENTIFIER | primary subscript | primary access ;
    */
    pub fn is_lvalue(&self) -> bool {
        // if there are no access items, then we are in the IDENTIFIER branch
        if self.path.is_empty() {
            matches!(self.atom, Atom::Identifier(..))
        } else {
            let last_op = self.path.last().unwrap();
            
            matches!(last_op, AccessItem::Member(..) | AccessItem::Index(..))
        }
    }
    
    pub fn push_access_member(&mut self, name: &str, interner: &mut StringInterner) {
        self.path.push(AccessItem::Member(InternStr::from_str(name, interner)))
    }
    
    pub fn push_access_index(&mut self, expr: Expr) {
        self.path.push(AccessItem::Index(Box::new(expr)))
    }
    
    //pub fn push_invoke(&mut self, )
    
    pub fn push_construct(&mut self, ctor: ObjectConstructor) {
        self.path.push(AccessItem::Construct(ctor))
    }
}


// Identifiers

// TODO intern all identifier names and string literals and make this Copy
// should just be a lightweight handle to an interned string
#[derive(Debug, Clone)]
pub struct InternStr<S=DefaultSymbol> where S: Symbol {
    symbol: S,
}

impl<S> InternStr<S> where S: Symbol {
    pub fn from_str<B>(s: &str, interner: &mut StringInterner<B>) -> Self
    where B: Backend<Symbol=S> {
        InternStr { symbol: interner.get_or_intern(s) }
    }
}

impl fmt::Display for InternStr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        // fmt.write_str(self.s)
        unimplemented!()
    }
}
