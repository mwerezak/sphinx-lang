use std::fmt;
use std::hash::{Hash, Hasher, BuildHasher};
use std::cmp::{PartialEq, Eq};
use crate::language::{IntType, FloatType};
use crate::runtime::strings::{StringValue, StringKey, StringTableGuard};
use crate::runtime::errors::{ExecResult, RuntimeErrorKind as ErrorKind};


// Fundamental data value type
#[derive(Debug, Clone)]
pub enum Variant {
    Nil,
    EmptyTuple, // the empty tuple value
    BoolTrue,
    BoolFalse,
    Integer(IntType),
    Float(FloatType),
    String(StringValue),
    //Tuple(GCHandle),
    //Object(GCHandle),
}

impl Variant {
    
    // Only "nil" and "false" have a truth value of false.
    pub fn truth_value(&self) -> bool {
        !matches!(self, Self::Nil | Self::BoolFalse)
    }
    
    // Note, bit_value() and float_value() are defined based on what is needed for the language *implementation*
    // They do not reflect the semantics of the Sphinx language
    
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
            // it's okay if this is a lossy conversion
            Self::Integer(value) => (*value) as FloatType,
            Self::Float(value) => *value,
            
            _ => panic!("float_value() valid only for numeric primitives"),
        }
    }
    
    // write a string representation of this value
    pub fn write_repr(&self, buf: &mut impl fmt::Write, string_table: &StringTableGuard) -> fmt::Result {
        match self {
            Self::Nil => buf.write_str("nil"),
            Self::EmptyTuple => buf.write_str("()"),
            Self::BoolTrue => buf.write_str("true"),
            Self::BoolFalse => buf.write_str("false"),
            
            Self::Integer(value) => write!(buf, "{}", *value),
            Self::Float(value) => {
                if value.trunc() != *value {
                    write!(buf, "{}", *value)
                } else {
                    write!(buf, "{}.0", value)
                }
            },
            
            Self::String(strval) => {
                buf.write_char('"')?;
                strval.write_str(buf, string_table)?;
                buf.write_char('"')
            },
        }
    }
    
    pub fn into_key<'s>(self, string_table: &'s StringTableGuard) -> ExecResult<VariantKey<'s>> {
        VariantKey::new(self, string_table)
    }
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

impl From<StringValue> for Variant {
    fn from(strval: StringValue) -> Self { Variant::String(strval) }
}



// Wrapper type for use as keys in VariantMap

#[derive(Debug, Clone)]
pub enum VariantKey<'s> {
    Nil,
    EmptyTuple, // the empty tuple value
    BoolTrue,
    BoolFalse,
    Integer(IntType),
    String(StringKey<'s>)
    // Tuple
    // Object(u64, GCHandle), // use user-defined hash and equality
}

impl Hash for VariantKey<'_> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        let discriminant = std::mem::discriminant(self);
        discriminant.hash(state);
        
        match self {
            Self::Integer(value) => value.hash(state),
            Self::String(strkey) => strkey.hash(state),
            _ => { }
        }
    }
}

impl Eq for VariantKey<'_> { }

impl<'s> PartialEq for VariantKey<'s> {
    fn eq(&self, other: &VariantKey<'s>) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::EmptyTuple, Self::EmptyTuple) => true,
            (Self::BoolTrue, Self::BoolTrue) => true,
            (Self::BoolFalse, Self::BoolFalse) => true,
            
            (Self::Integer(self_value), Self::Integer(other_value)) 
                => self_value == other_value,
            
            (Self::String(self_str), Self::String(other_str)) 
                => self_str == other_str,
                
            _ => false,
        }
    }
}

impl<'s> VariantKey<'s> {
    pub fn new(value: Variant, string_table: &'s StringTableGuard) -> ExecResult<Self> {
        let key = match value {
            Variant::Nil => Self::Nil,
            Variant::EmptyTuple => Self::EmptyTuple,
            Variant::BoolTrue => Self::BoolTrue,
            Variant::BoolFalse => Self::BoolFalse,
            
            Variant::Integer(value) => Self::Integer(value),
            Variant::String(strval) => Self::String(StringKey::new(strval, string_table)),
            
            Variant::Float(..) => return Err(ErrorKind::UnhashableType.into()),  // for now
            // Object - check if __hash is available
        };
        Ok(key)
    }
}

impl From<VariantKey<'_>> for Variant {
    fn from(key: VariantKey) -> Self {
        match key {
            VariantKey::Nil => Self::Nil,
            VariantKey::EmptyTuple => Self::EmptyTuple,
            VariantKey::BoolTrue => Self::BoolTrue,
            VariantKey::BoolFalse => Self::BoolFalse,
            VariantKey::Integer(value) => Self::Integer(value),
            VariantKey::String(value) => Self::String(value.into()),
        }
    }
}