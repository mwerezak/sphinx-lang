use core::fmt;
use core::hash::{Hash, Hasher};
use core::cmp::{PartialEq, Eq};
use static_assertions::assert_eq_size;
use crate::language::{IntType, FloatType};
use crate::runtime::types::{Tuple, UserData};
use crate::runtime::function::{Function, NativeFunction};
use crate::runtime::strings::{StringValue, StringSymbol, InlineStr, GCStr};
use crate::runtime::gc::{Gc, GcTrace};
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
    
    // separate different string types here to keep size down
    InternStr(StringSymbol),
    InlineStr(InlineStr),
    GCStr(GCStr),
    
    Tuple(Tuple),
    Function(Gc<Function>),
    NativeFunction(Gc<NativeFunction>),
    
    UserData(Gc<dyn UserData>),
}

impl Variant {
    
    pub fn as_strval(&self) -> Option<StringValue> {
        match self {
            Self::InternStr(symbol) => Some(StringValue::from(*symbol)),
            Self::InlineStr(inline) => Some(StringValue::from(*inline)),
            Self::GCStr(gc_str) => Some(StringValue::from(*gc_str)),
            _ => None,
        }
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
    fn from(value: StringValue) -> Self {
        match value {
            StringValue::Intern(symbol) => Self::InternStr(symbol),
            StringValue::Inline(inline) => Self::InlineStr(inline),
            StringValue::Gc(gc_str) => Self::GCStr(gc_str),
        }
    }
}

impl From<StringSymbol> for Variant {
    fn from(symbol: StringSymbol) -> Self {
        Self::InternStr(symbol)
    }
}

impl From<&str> for Variant {
    fn from(string: &str) -> Self {
        StringValue::from(string).into()
    }
}

impl From<Box<[Variant]>> for Variant {
    fn from(items: Box<[Variant]>) -> Self {
        Self::Tuple(items.into())
    }
}

impl From<Function> for Variant {
    fn from(func: Function) -> Self {
        Self::Function(Gc::new(func))
    }
}

impl From<NativeFunction> for Variant {
    fn from(func: NativeFunction) -> Self {
        Self::NativeFunction(Gc::new(func))
    }
}

unsafe impl GcTrace for Variant {
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
            
            Self::Function(fun) => (discr, fun).hash(state),
            Self::NativeFunction(fun) => (discr, fun).hash(state),
            Self::Tuple(items) => {
                discr.hash(state); // also prevent prefix collisions
                for item in items.as_ref().iter() {
                    item.try_hash(state)?;
                }
            },
            
            Self::InternStr(..) | Self::InlineStr(..) | Self::GCStr(..) =>
                self.as_strval().unwrap().hash(state),
            
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
        self.0.cmp_eq(other.0).unwrap_or(false)
    }
}
impl Eq for VariantKey<'_> { }

impl fmt::Display for Variant {
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if let Some(strval) = self.as_strval() {
            return write!(fmt, "{}", strval)
        }
        
        match self {
            Self::Nil => fmt.write_str("nil"),
            Self::BoolTrue => fmt.write_str("true"),
            Self::BoolFalse => fmt.write_str("false"),
            
            Self::InternStr(..) | Self::InlineStr(..) | Self::GCStr(..) => unreachable!(),
            
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
                Gc::as_id(fun),
            ),
            
            Self::NativeFunction(fun) => write!(
                fmt, "<built-in {} at {:#X}>",
                fun.signature().display_short(),
                Gc::as_id(fun),
            ),
            
            Self::UserData(data) =>
                if let Ok(name) = data.type_name() {
                    write!(fmt, "<{} at {:#X}>", name, Gc::as_id(data))
                } else {
                    write!(fmt, "<userdata at {:#X}>", Gc::as_id(data))
                }
        }
    }
}


/// Alternative display for Variant, kind of like repr() vs str() in Python
struct EchoDisplay<'a>(&'a Variant);

impl fmt::Display for EchoDisplay<'_> {
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if let Some(strval) = self.0.as_strval() {
            return write!(fmt, "\"{}\"", strval)
        }
        write!(fmt, "{}", self.0)
    }
}