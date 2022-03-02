use std::fmt;
use crate::language::{IntType, FloatType};
use crate::runtime::data::InternStr;


// Fundamental data value type
#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Nil,
    EmptyTuple, // the empty tuple value
    Boolean(bool),
    Integer(IntType),
    Float(FloatType),
    InternStr(InternStr),
    //GCObject(GCHandle),
}

impl Variant {
    // Only "nil" and "false" have a truth value of false.
    pub fn truth_value(&self) -> bool {
        !matches!(self, Self::Nil | Self::Boolean(false))
    }
    
    // Note, bit_value() and float_value() are defined based on what is needed for the language *implementation*
    // They do not reflect the semantics of the ReLox language
    
    pub fn bit_value(&self) -> IntType {
        match self {
            Self::Integer(value) => *value,
            Self::Boolean(false) => 0, // all 0s
            Self::Boolean(true) => !0, // all 1s
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

impl fmt::Display for Variant {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => fmt.write_str("nil"),
            Self::EmptyTuple => fmt.write_str("()"),
            Self::Boolean(true) => fmt.write_str("true"),
            Self::Boolean(false) => fmt.write_str("false"),
            Self::Integer(value) => write!(fmt, "{}", value),
            Self::Float(value) => write!(fmt, "{:.6}", value),
            Self::InternStr(sym) => write!(fmt, "$({:?})", sym.symbol()),
        }
    }
}
