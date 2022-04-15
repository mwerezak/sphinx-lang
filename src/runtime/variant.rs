use core::fmt;
use core::cell::Cell;
use core::hash::{Hash, Hasher};
use core::cmp::{PartialEq, Eq};
use static_assertions::assert_eq_size;
use crate::language::{IntType, FloatType};
use crate::runtime::types::{Type, Tuple};
use crate::runtime::function::{Function, NativeFunction, Call};
use crate::runtime::strings::{StringValue, StringSymbol};
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};

#[cfg(target_arch = "x86_64")]
assert_eq_size!(Variant, [u8; 24]);

// Fundamental data value type
#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Nil,
    BoolTrue,
    BoolFalse,
    
    Integer(IntType),
    Float(FloatType),
    String(StringValue),
    
    Tuple(Tuple), // TODO: stop using Box when DST support is stabilized
    Function(GC<Function>),
    NativeFunction(GC<NativeFunction>),
}

impl Variant {
    pub fn as_gc(&self) -> Option<GC<dyn GCTrace>> {
        GC::<dyn GCTrace>::try_from(self).ok()
    }
    
    pub fn echo(&self) -> impl fmt::Display + '_ {
        EchoDisplay(self)
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

impl From<StringValue> for Variant {
    fn from(value: StringValue) -> Self { Self::String(value) }
}

impl From<StringSymbol> for Variant {
    fn from(symbol: StringSymbol) -> Self {
        Self::String(symbol.into())
    }
}

impl From<&str> for Variant {
    fn from(string: &str) -> Self {
        Self::String(string.into())
    }
}

impl From<Box<[Variant]>> for Variant {
    fn from(items: Box<[Variant]>) -> Self {
        Self::Tuple(items.into())
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
    #[inline]
    fn trace(&self) {
        match self {
            Self::Tuple(tuple) => tuple.trace(),
            Self::Function(fun) => fun.mark_trace(),
            Self::NativeFunction(fun) => fun.mark_trace(),
            _ => { },
        };
    }
}


// extract the GC handle for GC'd types
impl TryFrom<&Variant> for GC<dyn GCTrace> {
    type Error = ();
    
    fn try_from(value: &Variant) -> Result<Self, Self::Error> {
        match value {
            Variant::Tuple(tuple) => tuple.as_gc().ok_or(()),
            Variant::Function(fun) => Ok((*fun).into()),
            Variant::NativeFunction(fun) => Ok((*fun).into()),
            _ => Err(()),
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
        let discr = core::mem::discriminant(self);
        
        match self {
            Self::Nil | Self::BoolTrue | Self::BoolFalse 
                => discr.hash(state),
            
            Self::Integer(value) => (discr, value).hash(state),
            Self::String(value) => (discr, value).hash(state),
            Self::Function(fun) => (discr, fun).hash(state),
            Self::NativeFunction(fun) => (discr, fun).hash(state),
            Self::Tuple(items) => {
                discr.hash(state); // also prevent prefix collisions
                for item in items.as_ref().iter() {
                    item.try_hash(state)?;
                }
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
    type Error = Box<RuntimeError>;
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
            (Variant::BoolTrue, Variant::BoolTrue) => true,
            (Variant::BoolFalse, Variant::BoolFalse) => true,
            
            (Variant::Integer(a), Variant::Integer(b)) => a == b,
            (Variant::String(a), Variant::String(b)) => a == b,
            
            (Variant::Tuple(a), Variant::Tuple(b)) if a.is_empty() && b.is_empty() => true,
            (Variant::Tuple(a), Variant::Tuple(b)) => {
                a.len() == b.len() && (
                    a.as_ref().iter().zip(b.as_ref().iter())
                        .all(|(a, b)| VariantKey(a) == VariantKey(b))
                )
            },
            
            (Variant::Function(a), Variant::Function(b)) => GC::ptr_eq(a, b),
            (Variant::NativeFunction(a), Variant::NativeFunction(b)) => GC::ptr_eq(a, b),
            
            _ => false,
        }
    }
}
impl Eq for VariantKey<'_> { }

impl fmt::Display for Variant {
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Nil => fmt.write_str("nil"),
            Self::BoolTrue => fmt.write_str("true"),
            Self::BoolFalse => fmt.write_str("false"),
            
            Self::String(value) => write!(fmt, "{}", value),
            Self::Tuple(tuple) => write!(fmt, "{}", tuple),
            
            Self::Integer(value) => write!(fmt, "{}", *value),
            Self::Float(value) => {
                if !value.is_finite() {
                    write!(fmt, "{}", *value)
                } else if value.trunc() != *value {
                    write!(fmt, "{}", *value)
                } else {
                    write!(fmt, "{}.0", value)
                }
            },
            
            Self::Function(fun) => write!(
                fmt, "<{} at {:#X}>",
                fun.signature().display_short(), 
                GC::as_id(fun),
            ),
            
            Self::NativeFunction(fun) => write!(
                fmt, "<built-in {} at {:#X}>",
                fun.signature().display_short(),
                GC::as_id(fun),
            ),
        }
    }
}


/// Alternative display for Variant, kind of like repr() vs str() in Python
struct EchoDisplay<'a>(&'a Variant);

impl fmt::Display for EchoDisplay<'_> {
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.0 {
            Variant::String(value) => write!(fmt, "\"{}\"", value),
            _ => write!(fmt, "{}", self.0),
        }
    }
}