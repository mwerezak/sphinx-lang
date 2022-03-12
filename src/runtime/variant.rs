use std::fmt;
use std::hash::{Hash, Hasher};
use std::cmp::{PartialEq, Eq};
use crate::language::{IntType, FloatType};
use crate::runtime::strings::{StringValue, StringKey};
use crate::runtime::string_table::StringTable;
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
    //Tuple(Tuple),
    //Object(GCHandle),
}

impl Variant {
    
    // Only "nil" and "false" have a truth value of false.
    pub fn truth_value(&self) -> bool {
        !matches!(self, Self::Nil | Self::BoolFalse)
    }
    
    // Note, bit_value() and float_value() are defined based on what is needed for the language *implementation*
    // They do not reflect the semantics of the Sphinx language
    
    pub fn bit_value(&self) -> Option<IntType> {
        let value = match self {
            Self::Integer(value) => *value,
            Self::BoolFalse => 0, // all 0s
            Self::BoolTrue => !0, // all 1s
            _ => return None,
        };
        Some(value)
    }
    
    pub fn float_value(&self) -> Option<FloatType> {
        let value = match self {
            // it's okay if this is a lossy conversion
            Self::Integer(value) => (*value) as FloatType,
            Self::Float(value) => *value,
            _ => return None,
        };
        Some(value)
    }
    
    pub fn into_key<'s>(self, string_table: &'s StringTable) -> ExecResult<VariantKey<'s>> {
        VariantKey::new(self, string_table)
    }
    
    pub fn repr<'a, 's>(&'a self, string_table: &'s StringTable) -> impl fmt::Display + 'a where 's: 'a {
        VariantRepr(self, string_table)
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
    pub fn new(value: Variant, string_table: &'s StringTable) -> ExecResult<Self> {
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


struct VariantRepr<'a, 's>(&'a Variant, &'s StringTable);

impl fmt::Display for VariantRepr<'_, '_> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let VariantRepr(value, string_table) = self;
        match value {
            Variant::Nil => fmt.write_str("nil"),
            Variant::EmptyTuple => fmt.write_str("()"),
            Variant::BoolTrue => fmt.write_str("true"),
            Variant::BoolFalse => fmt.write_str("false"),
            
            Variant::Integer(value) => write!(fmt, "{}", *value),
            Variant::Float(value) => {
                if value.trunc() != *value {
                    write!(fmt, "{}", *value)
                } else {
                    write!(fmt, "{}.0", value)
                }
            },
            
            Variant::String(strval) => {
                let s = &strval.as_str(string_table) as &str;
                write!(fmt, "\"{}\"", s)
            },
        }
    }
}



