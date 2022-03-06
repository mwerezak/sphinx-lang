use std::fmt;
use std::hash::{Hash, Hasher, BuildHasher};
use std::cmp::{PartialEq, Eq};
use std::collections::HashMap;
use crate::language::{IntType, FloatType};
use crate::runtime::Runtime;
use crate::runtime::data::{InternSymbol, StringRepr, DefaultBuildHasher};
use crate::runtime::errors::{ExecResult, RuntimeErrorKind as ErrorKind};


pub type VariantMap<'r> = HashMap<VariantKey<'r>, Variant, DefaultBuildHasher>;

// Fundamental data value type
#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Nil,
    EmptyTuple, // the empty tuple value
    BoolTrue,
    BoolFalse,
    Integer(IntType),
    Float(FloatType),
    InternStr(InternSymbol),  // TODO include reference to string table
    //StrObject(GCHandle),
    //Tuple(GCHandle),
    //Object(GCHandle),
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
            // it'r okay if this is a lossy conversion
            Self::Integer(value) => (*value) as FloatType,
            Self::Float(value) => *value,
            
            _ => panic!("float_value() valid only for numeric primitives"),
        }
    }
    
    // write a string representation of this value
    pub fn write_repr(&self, dst: &mut impl fmt::Write, runtime: &Runtime) -> fmt::Result {
        match self {
            Self::Nil => dst.write_str("nil"),
            Self::EmptyTuple => dst.write_str("()"),
            Self::BoolTrue => dst.write_str("true"),
            Self::BoolFalse => dst.write_str("false"),
            Self::Integer(value) => write!(dst, "{}", *value),
            Self::Float(value) => write!(dst, "{}", *value),
            Self::InternStr(sym) => {
                let sym = (*sym).into();
                let string = runtime.string_table().resolve(sym).unwrap();
                write!(dst, "\"{}\"", string)
            },
        }
    }
    
    pub fn into_key<'r>(self, runtime: &'r Runtime, hasher_factory: &impl BuildHasher) -> ExecResult<VariantKey<'r>> {
        VariantKey::new(self, runtime, hasher_factory)
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

impl From<InternSymbol> for Variant {
    fn from(sym: InternSymbol) -> Self { Variant::InternStr(sym) }
}



// Wrapper type for use as keys in VariantMap

#[derive(Debug, Clone, Copy)]
pub enum VariantKey<'r> {
    Nil,
    EmptyTuple, // the empty tuple value
    BoolTrue,
    BoolFalse,
    Integer(IntType),
    String(u64, StringRepr<'r>)
    // Tuple
    // Object(u64, GCHandle), // use user-defined hash and equality
}

impl Hash for VariantKey<'_> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        let discriminant = std::mem::discriminant(self);
        discriminant.hash(state);
        
        match self {
            Self::Integer(value) => value.hash(state),
            Self::String(strhash, _) => strhash.hash(state),
            _ => { }
        }
    }
}

impl Eq for VariantKey<'_> { }

impl<'r> PartialEq for VariantKey<'r> {
    fn eq(&self, other: &VariantKey<'r>) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::EmptyTuple, Self::EmptyTuple) => true,
            (Self::BoolTrue, Self::BoolTrue) => true,
            (Self::BoolFalse, Self::BoolFalse) => true,
            
            (Self::Integer(self_value), Self::Integer(other_value)) 
                => self_value == other_value,
            
            (Self::String(.., self_string), Self::String(.., other_string)) 
                => self_string == other_string,
                
            _ => false,
        }
    }
}

impl<'r> VariantKey<'r> {
    pub fn new(value: Variant, runtime: &'r Runtime, hasher_factory: &impl BuildHasher) -> ExecResult<Self> {
        let key = match value {
            Variant::Nil => Self::Nil,
            Variant::EmptyTuple => Self::EmptyTuple,
            Variant::BoolTrue => Self::BoolTrue,
            Variant::BoolFalse => Self::BoolFalse,
            
            Variant::Integer(value) => Self::Integer(value),
            
            Variant::InternStr(sym) => {
                let string = StringRepr::InternStr(sym, runtime.string_table());
                
                let mut hasher = hasher_factory.build_hasher();
                string.as_str().hash(&mut hasher);
                let hash = hasher.finish();
                
                Self::String(hash, string)
            },
            
            Variant::Float(..) => return Err(ErrorKind::UnhashableType.into()),  // for now
            // Object - check if __hash is available
        };
        Ok(key)
    }
    
}