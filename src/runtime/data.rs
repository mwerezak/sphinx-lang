use std::fmt;
use string_interner::{Symbol, StringInterner};
use string_interner::backend::Backend;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;

use crate::language;
use crate::runtime::types::RuntimeType;


// Fundamental data value type
#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Nil,
    Empty, // the empty tuple value
    Boolean(bool),
    Integer(language::IntType),
    Real(language::FloatType),
    InternString(InternStr),
    //GCObject(GCHandle),
}

impl Variant {
    pub fn rtype(&self) -> &RuntimeType {
        unimplemented!()
    }
    
    pub fn truth_value(&self) -> bool {
        unimplemented!()
    }
}


// Interned strings

pub type StrSymbol = DefaultSymbol;
pub type StrBackend = DefaultBackend<StrSymbol>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct InternStr {
    symbol: StrSymbol,
}

impl InternStr {
    pub fn new(symbol: StrSymbol) -> Self {
        InternStr { symbol }
    }
    
    pub fn from_str(s: &str, interner: &mut StringInterner<StrBackend>) -> Self {
        InternStr { symbol: interner.get_or_intern(s) }
    }
    
    pub fn symbol(&self) -> StrSymbol { self.symbol }
}

