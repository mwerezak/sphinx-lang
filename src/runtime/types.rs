use std::fmt;
use once_cell::sync::Lazy;

use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::errors::{ExecResult, ErrorKind};

pub mod operator;
pub mod metatable;
pub mod dispatch;
pub mod numeric;


// Type tag for Sphinx's "primitive" types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Boolean,
    Integer,
    Float,
    String,
    Tuple,
    Function,
    Metatable,
    Object,
}


impl fmt::Display for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Nil => "nil",
            Self::Boolean => "bool",
            Self::Integer => "int",
            Self::Float => "float",
            Self::String => "string",
            Self::Tuple => "tuple",
            Self::Function => "function",
            Self::Metatable => "metatable",
            
            // note, when looking up the type of an object value 
            // the object's metatable should be used to generate the type name
            Self::Object => "object",
        };
        fmt.write_str(name)
    }
}



#[allow(unused_variables)]
pub trait MetaObject {
    fn type_tag(&self) -> Type;
    
    // primitive coercions
    fn as_bool(&self) -> ExecResult<bool> { Ok(true) }
    fn as_bits(&self) -> Option<ExecResult<IntType>> { None }
    fn as_int(&self) -> Option<ExecResult<IntType>> { None }
    fn as_float(&self) -> Option<ExecResult<FloatType>> { None }
    
    // unary operators
    fn apply_neg(&self) -> Option<ExecResult<Variant>> { None }
    fn apply_pos(&self) -> Option<ExecResult<Variant>> { None }
    fn apply_inv(&self) -> Option<ExecResult<Variant>> { None }

    // arithmetic operators
    fn apply_mul(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rmul(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_div(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rdiv(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_mod(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rmod(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_sub(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rsub(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
}

impl Variant {
    pub fn as_meta(&self) -> &dyn MetaObject {
        match self {
            Self::Nil => &(),
            Self::BoolTrue => &true,
            Self::BoolFalse => &false,
            Self::Integer(value) => value,
            Self::Float(value) => value,
            _ => &(),
        }
    }
    
    // pub fn type_tag(&self) -> Type {
    //     self.as_meta().type_tag()
    // }
    
    pub fn as_bool(&self) -> ExecResult<bool> { 
        self.as_meta().as_bool()
    }
    
    pub fn as_bits(&self) -> ExecResult<IntType> {
        self.as_meta().as_bits()
            .ok_or_else(|| ErrorKind::CantInterpretAsBits(*self))?
    }
    
    pub fn as_int(&self) -> ExecResult<IntType> {
        self.as_meta().as_int()
            .ok_or_else(|| ErrorKind::CantInterpretAsInt(*self))?
    }
    
    pub fn as_float(&self) -> ExecResult<FloatType> {
        self.as_meta().as_float()
            .ok_or_else(|| ErrorKind::CantInterpretAsFloat(*self))?
    }
}


impl MetaObject for () {
    fn type_tag(&self) -> Type { Type::Nil }
    
    fn as_bool(&self) -> ExecResult<bool> { Ok(false) }
}

impl MetaObject for bool {
    fn type_tag(&self) -> Type { Type::Boolean }
    
    fn as_bool(&self) -> ExecResult<bool> { Ok(*self) }
    
    fn as_bits(&self) -> Option<ExecResult<IntType>> {
        if *self { Some(Ok(!0)) } // all 1s
        else { Some(Ok(0)) } // all 0s
    }
    
    fn apply_inv(&self) -> Option<ExecResult<Variant>> { 
        Some(Ok(Variant::from(!(*self)))) 
    }
}