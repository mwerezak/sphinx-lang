
use crate::language::{IntType, FloatType};
use crate::runtime::data::InternStr;
use crate::runtime::errors::{EvalResult, EvalErrorKind};

// marker for the fundamental type of a value (from the language's perspective)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
    Nil,
    Boolean,
    Integer,
    Float,
    String,
    Tuple,
    Object,
}

impl Primitive {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::Integer | Self::Float)
    }
}


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


// impl PartialEq for Variant { }
// impl PartialOrd for Variant { }

impl Variant {
    // Only "nil" and "false" have a truth value of false.
    pub fn truth_value(&self) -> bool {
        !matches!(self, Self::Nil | Self::Boolean(false))
    }
    
    pub fn int_value(&self) -> EvalResult<IntType> {
        let value = match self {
            Self::Integer(value) => *value,
            Self::Float(value) => (*value as IntType),  // TODO revisit
            _ => unimplemented!(),
        };
        Ok(value)
    }
    
    pub fn float_value(&self) -> EvalResult<FloatType> {
        let value = match self {
            Self::Integer(value) => FloatType::from(*value),
            Self::Float(value) => *value,
            _ => unimplemented!(),
        };
        Ok(value)
    }
    
    // pub fn string_value(&self)
    
    pub fn pri_type(&self) -> Primitive {
        match self {
            Self::Nil => Primitive::Nil,
            Self::EmptyTuple => Primitive::Tuple,
            Self::Boolean(..) => Primitive::Boolean,
            Self::Integer(..) => Primitive::Integer,
            Self::Float(..) => Primitive::Float,
            Self::InternStr(..) => Primitive::String,
        }
    }
    
}


