use std::fmt;
use string_interner::{Symbol, StringInterner};
use string_interner::backend::Backend;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;

use crate::language;
use crate::runtime::Runtime;
use crate::runtime::types::RuntimeType;
use crate::runtime::errors::{RuntimeResult, RuntimeError, ErrorKind};

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Primitive {
    Nil,
    Boolean,
    Integer,
    Float,
    String,
    Tuple,
    Object,
}

// Fundamental data value type
#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Nil,
    Empty, // the empty tuple value
    Boolean(bool),
    Integer(language::IntType),
    Float(language::FloatType),
    InternString(InternStr),
    //GCObject(GCHandle),
}

impl Variant {
    pub fn rtype<'r>(&self, runtime: &'r Runtime) -> &'r RuntimeType {
        match self {
            Self::Integer(..) => runtime.get_primitive_type(Primitive::Integer),
            _ => unimplemented!(),
        }
    }
    
    pub fn truth_value(&self) -> bool {
        unimplemented!()
    }
    
    pub fn int_value(&self) -> Option<language::IntType> {
        match *self {
            Self::Integer(value) => Some(value),
            Self::Float(value) if !value.is_nan() => Some(value as language::IntType),
            _ => None,
        }
    }
    
    pub fn float_value(&self) -> Option<language::FloatType> {
        match *self {
            Self::Integer(value) => Some(value as language::FloatType),
            Self::Float(value) => Some(value),
            _ => None,
        }
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

