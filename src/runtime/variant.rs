use std::fmt;
use std::cell::Cell;
use std::hash::{Hash, Hasher};
use std::cmp::{PartialEq, Eq};
use static_assertions::assert_eq_size;
use crate::language::{IntType, FloatType};
use crate::runtime::types::Type;
use crate::runtime::types::metatable::Metatable;
use crate::runtime::function::{Function, NativeFunction, Call, Invoke};
use crate::runtime::types::primitive::*;
use crate::runtime::strings::StringSymbol;
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};

#[cfg(target_arch = "x86_64")]
assert_eq_size!(Variant, [u8; 16]);

// Fundamental data value type
#[derive(Clone, Copy)]
pub enum Variant {
    Nil,
    EmptyTuple,
    BoolTrue,
    BoolFalse,
    
    Integer(IntType),
    Float(FloatType),
    String(StringSymbol),
    
    Tuple(GC<Box<[Variant]>>), // TODO: stop using Box when DST support is stabilized
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
    
    pub fn invoke(&self, args: &[Variant]) -> ExecResult<Call> {
        match self {
            Self::Function(fun) => fun.invoke(args),
            Self::NativeFunction(fun) => fun.invoke(args),
            
            _ => Err(ErrorKind::NotCallable(*self).into())
        }
    }
    
    pub fn as_gc(self) -> Option<GC<dyn GCTrace>> {
        GC::<dyn GCTrace>::try_from(self).ok()
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
        Self::Tuple(GC::new(items))
    }
}

impl From<Function> for Variant {
    fn from(func: Function) -> Self {
        Self::Function(GC::new(func))
    }
}

impl From<NativeFunction> for Variant {
    fn from(func: NativeFunction) -> Self {
        Self::NativeFunction(GC::new(func))
    }
}

unsafe impl GCTrace for Variant {
    fn trace(&self) {
        match self {
            Self::Tuple(items) => items.mark_trace(),
            Self::Function(fun) => fun.mark_trace(),
            Self::NativeFunction(fun) => fun.mark_trace(),
            _ => { },
        };
    }
}

unsafe impl GCTrace for Cell<Variant> {
    fn trace(&self) {
        self.get().trace()
    }
}

unsafe impl GCTrace for Box<[Variant]> {
    fn trace(&self) {
        for item in self.iter() {
            item.trace();
        }
    }
}


// extract the GC handle for GC'd types
impl TryFrom<Variant> for GC<dyn GCTrace> {
    type Error = Variant;
    
    fn try_from(value: Variant) -> Result<Self, Self::Error> {
        match value {
            Variant::Tuple(tuple) => Ok(tuple.into()),
            Variant::Function(fun) => Ok(fun.into()),
            Variant::NativeFunction(fun) => Ok(fun.into()),
            _ => Err(value),
        }
    }
}


// Not all Variants are hashable
impl Variant {
    pub fn can_hash(&self) -> bool {
        struct DummyHasher();
        impl Hasher for DummyHasher {
            fn finish(&self) -> u64 { 0 }
            fn write(&mut self, _: &[u8]) { }
        }
        self.try_hash(&mut DummyHasher()).is_ok()
    }
    
    fn try_hash<H: Hasher>(&self, state: &mut H) -> ExecResult<()> {
        let disc = std::mem::discriminant(self);
        
        match self {
            Self::Nil | Self::BoolTrue | Self::BoolFalse | Self::EmptyTuple 
                => disc.hash(state),
            
            Self::Integer(value) => (disc, value).hash(state),
            Self::String(value) => (disc, value).hash(state),
            Self::Function(fun) => (disc, fun).hash(state),
            Self::NativeFunction(fun) => (disc, fun).hash(state),
            Self::Tuple(items) => {
                disc.hash(state);
                for item in items.iter() {
                    item.try_hash(state)?;
                }
                0xFF.hash(state); // to avoid prefix collisions
            },
            
            _ => return Err(ErrorKind::UnhashableValue(*self).into()),
        }
        Ok(())
    }
    
}


/// Wrapper for variant that dynamically ensures hashability
#[derive(Clone)]
pub struct VariantKey<'a>(&'a Variant);

impl<'a> TryFrom<&'a Variant> for VariantKey<'a> {
    type Error = RuntimeError;
    fn try_from(value: &'a Variant) -> ExecResult<Self> {
        if !value.can_hash() {
            return Err(ErrorKind::UnhashableValue(*value).into());
        }
        Ok(Self(value))
    }
}

impl Hash for VariantKey<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.try_hash(state).unwrap()
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
            
            (Variant::Function(a), Variant::Function(b)) => GC::ptr_eq(a, b),
            (Variant::NativeFunction(a), Variant::NativeFunction(b)) => GC::ptr_eq(a, b),
            
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




