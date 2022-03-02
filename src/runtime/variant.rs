use std::fmt;
use crate::language::{IntType, FloatType};
use crate::runtime::data::InternStr;


// Fundamental data value type
#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Nil,
    EmptyTuple, // the empty tuple value
    BoolTrue,
    BoolFalse,
    Integer(IntType),
    Float(FloatType),
    InternStr(InternStr),
    //GCObject(GCHandle),
}

impl Variant {
    // Only "nil" and "false" have a truth value of false.
    pub fn truth_value(&self) -> bool {
        !matches!(self, Self::Nil | Self::BoolFalse)
    }
    
    // Note, bit_value() and float_value() are defined based on what is needed for the language *implementation*
    // They do not reflect the semantics of the ReLox language
    
    pub fn bit_value(&self) -> IntType {
        match self {
            Self::Integer(value) => *value,
            Self::BoolFalse => 0, // all 0s
            Self::BoolTrue => !0, // all 1s
            _ => panic!("bit_value() valid only for integers and booleans"),
        }
    }
    
    pub fn float_value(&self) -> FloatType {
        match self {
            // if we switch to 64-bit ints, this will become a lossy conversion
            // this method should always succeed for numeric primitive types
            Self::Integer(value) => (*value) as FloatType,
            Self::Float(value) => *value,
            
            _ => panic!("float_value() valid only for numeric primitives"),
        }
    }
    
    // pub fn string_value(&self) -> ... { }

}


impl From<bool> for Variant {
    fn from(value: bool) -> Self {
        match value {
            true => Self::BoolTrue,
            false => Self::BoolFalse,
        }
    }
}

impl From<IntType> for Variant {
    fn from(value: IntType) -> Self { Variant::Integer(value) }
}

impl From<FloatType> for Variant {
    fn from(value: FloatType) -> Self { Variant::Float(value) }
}

impl From<InternStr> for Variant {
    fn from(intern: InternStr) -> Self { Variant::InternStr(intern) }
}
