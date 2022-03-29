use std::fmt;
use std::rc::Rc;
use std::hash::{Hash, Hasher};
use std::cmp::{PartialEq, Eq};
use crate::language::{IntType, FloatType};
use crate::runtime::types::Type;
use crate::runtime::types::metatable::Metatable;
use crate::runtime::types::function::{Function, NativeFunction, Call, Invoke};
use crate::runtime::types::primitive::*;
use crate::runtime::strings::{StringSymbol, STRING_TABLE};
use crate::runtime::gc::{GC, GCArray, GCTrace};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


// Fundamental data value type
#[derive(Clone)] // add Copy?
pub enum Variant {
    Nil,
    EmptyTuple,
    BoolTrue,
    BoolFalse,
    
    Integer(IntType),
    Float(FloatType),
    String(StringSymbol),
    
    Tuple(GCArray<Variant>),
    Function(GC<Function>),
    NativeFunction(GC<NativeFunction>),
}

impl Variant {
    pub fn get_type(&self) -> Type {
        match self {
            Self::Nil => Type::Nil,
            Self::BoolTrue => Type::Boolean,
            Self::BoolFalse => Type::Boolean,
            Self::Integer(..) => Type::Integer,
            Self::Float(..) => Type::Float,
            Self::String(..) => Type::String,
            Self::EmptyTuple => Type::Tuple,
            Self::Tuple(..) => Type::Tuple,
            Self::Function(..) => Type::Function,
            Self::NativeFunction(..) => Type::Function,
        }
    }
    
    pub fn metatable(&self) -> &Metatable {
        match self {
            Self::String(..) => &METATABLE_STRING,
            _ => &METATABLE_DEFAULT,
        }
    }
    
    // Only "nil" and "false" have a truth value of false.
    pub fn truth_value(&self) -> bool {
        !matches!(self, Self::Nil | Self::BoolFalse)
    }
    
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
    
    pub fn can_hash(&self) -> bool {
        match self {
            Self::Float(..) => false,
            Self::Tuple(items) => items.iter().all(|item| item.can_hash()),
            // TODO Objects - check metatabale for __hash
            _ => true
        }
        
    }
    
    pub fn invoke(&self, args: &[Variant]) -> ExecResult<Call> {
        match self {
            Self::Function(fun) => fun.invoke(args),
            
            _ => Err(ErrorKind::NotCallable(self.clone()).into())
        }
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
    fn from(value: IntType) -> Self { Self::Integer(value) }
}

impl From<FloatType> for Variant {
    fn from(value: FloatType) -> Self { Self::Float(value) }
}

impl From<StringSymbol> for Variant {
    fn from(value: StringSymbol) -> Self { Self::String(value) }
}

impl From<&str> for Variant {
    fn from(string: &str) -> Self {
        Self::String(string.into())
    }
}

impl From<Box<[Variant]>> for Variant {
    fn from(items: Box<[Variant]>) -> Self {
        Self::Tuple(GCArray::from_boxed_slice(items))
    }
}

impl From<Function> for Variant {
    fn from(func: Function) -> Self {
        Self::Function(GC::allocate(func))
    }
}

impl GCTrace for Variant { }


// Not all Variants are hashable, so there is a separate type to handle that

#[derive(Clone)]
pub struct VariantKey<'a>(&'a Variant);

impl<'a> TryFrom<&'a Variant> for VariantKey<'a> {
    type Error = RuntimeError;
    fn try_from(value: &'a Variant) -> ExecResult<Self> {
        if !value.can_hash() {
            return Err(ErrorKind::UnhashableValue(value.clone()).into());
        }
        Ok(Self(value))
    }
}

impl Hash for VariantKey<'_> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        debug_assert!(self.0.can_hash());
        
        let discriminant = std::mem::discriminant(self.0);
        discriminant.hash(state);
        
        match self.0 {
            Variant::Integer(value) => value.hash(state),
            Variant::String(strkey) => strkey.hash(state),
            // TODO objects - get the result of __hash and hash it
            _ => { }
        }
    }
}

impl<'s> PartialEq for VariantKey<'_> {
    fn eq(&self, other: &VariantKey) -> bool {
        match (self.0, other.0) {
            (Variant::Nil, Variant::Nil) => true,
            (Variant::EmptyTuple, Variant::EmptyTuple) => true,
            (Variant::BoolTrue, Variant::BoolTrue) => true,
            (Variant::BoolFalse, Variant::BoolFalse) => true,
            
            (Variant::Integer(a), Variant::Integer(b)) => a == b,
            (Variant::String(a), Variant::String(b)) => a == b,
            
            (Variant::Tuple(a), Variant::Tuple(b)) if a.len() != b.len() => false,
            (Variant::Tuple(a), Variant::Tuple(b)) => {
                a.iter().zip(b.iter())
                .all(|(a, b)| VariantKey(a) == VariantKey(b))
            },
            
            // TODO objects, use __eq
            
            _ => false,
        }
    }
}
impl Eq for VariantKey<'_> { }

impl fmt::Debug for Variant {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => fmt.write_str("nil"),
            Self::EmptyTuple => fmt.write_str("()"),
            Self::BoolTrue => fmt.write_str("true"),
            Self::BoolFalse => fmt.write_str("false"),
            
            Self::Integer(value) => write!(fmt, "{}", *value),
            Self::Float(value) => {
                if value.trunc() != *value {
                    write!(fmt, "{}", *value)
                } else {
                    write!(fmt, "{}.0", value)
                }
            },
            
            Self::String(value) => write!(fmt, "\"{}\"", value),
            
            Self::Tuple(items) => {
                let (last, rest) = items.split_last().unwrap(); // will never be empty
                
                write!(fmt, "(")?;
                for item in rest.iter() {
                    write!(fmt, "{:?}, ", item)?;
                }
                write!(fmt, "{:?})", last)
            }
            
            Self::Function(fun) => write!(fmt, "<{}>", fun.signature()),
            Self::NativeFunction(fun) => write!(fmt, "<built-in {}>", fun.signature()),
        }
    }
}

impl fmt::Display for Variant {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(value) => write!(fmt, "{}", value),
            
            Self::Tuple(items) => {
                let (last, rest) = items.split_last().unwrap(); // will never be empty
                
                write!(fmt, "(")?;
                for item in rest.iter() {
                    write!(fmt, "{}, ", item)?;
                }
                write!(fmt, "{})", last)
            }
            
            // Self::Object(handle) => // TODO invoke __tostring
            
            _ => write!(fmt, "{}", self)
        }
    }
}




