use core::fmt;
use core::hash::{Hash, Hasher};
use static_assertions::const_assert_eq;
use crate::language::{IntType, FloatType};
use crate::runtime::types::{Tuple, UserData, UserIterator, Marker};
use crate::runtime::function::{Function, NativeFunction};
use crate::runtime::strings::{StringValue, StringSymbol, InlineStr};
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::errors::{ExecResult, RuntimeError};


// try to use less memory on 32-bit architectures
#[cfg(target_pointer_width = "32")]
const_assert_eq!(core::mem::size_of::<Variant>(), 8);

#[cfg(target_pointer_width = "64")]
const_assert_eq!(core::mem::size_of::<Variant>(), 16);

// Fundamental data value type
#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Nil,
    BoolTrue,
    BoolFalse,
    
    Marker(Marker),
    
    Integer(IntType),
    Float(FloatType),
    
    // separate different string types here to keep size down
    InternStr(StringSymbol),
    InlineStr(InlineStr),
    GCStr(Gc<str>),
    
    Tuple(Tuple),
    Function(Gc<Function>),
    NativeFunction(Gc<NativeFunction>),
    
    Iterator(Gc<dyn UserIterator>),
    
    UserData(Gc<dyn UserData>),
}

unsafe impl GcTrace for Variant {
    #[inline]
    fn trace(&self) {
        match self {
            Self::Tuple(tuple) => tuple.trace(),
            Self::Function(fun) => fun.mark_trace(),
            Self::NativeFunction(fun) => fun.mark_trace(),
            Self::Iterator(iter) => iter.mark_trace(),
            Self::UserData(data) => data.mark_trace(),
            _ => { },
        };
    }
}

impl Variant {
    pub fn marker(id: StringSymbol) -> Self {
        Self::Marker(Marker::new(id))
    }
    
    pub fn is_nil(&self) -> bool {
        matches!(self, Variant::Nil)
    }
    
    pub fn as_strval(&self) -> Option<StringValue> {
        match self {
            Self::InternStr(symbol) => Some(StringValue::from(*symbol)),
            Self::InlineStr(inline) => Some(StringValue::from(*inline)),
            Self::GCStr(gc_str) => Some(StringValue::from(*gc_str)),
            _ => None,
        }
    }
    
    pub fn display_echo(&self) -> impl fmt::Display + '_ {
        struct Display<'a>(&'a Variant);
        impl fmt::Display for Display<'_> {
            fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                match self.0.fmt_echo() {
                    Ok(strval) => write!(fmt, "{}", strval),
                    Err(error) => write!(fmt, "{}", error),
                }
            }
        }
        
        Display(self)
    }
    
    pub fn fmt_str(&self) -> ExecResult<StringValue> {
        if let Some(strval) = self.as_strval() {
            Ok(strval)
        } else {
            self.fmt_echo()
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

impl From<Gc<dyn UserIterator>> for Variant {
    fn from(iter: Gc<dyn UserIterator>) -> Self {
        Self::Iterator(iter)
    }
}

impl From<Gc<dyn UserData>> for Variant {
    fn from(data: Gc<dyn UserData>) -> Self {
        Self::UserData(data)
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
            
            _ => return Err(RuntimeError::unhashable_value(self)),
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
            return Err(RuntimeError::unhashable_value(value));
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
        match self.fmt_echo() {
            Ok(strval) => write!(fmt, "{}", strval),
            Err(error) => write!(fmt, "{}", error),
        }
    }
}
