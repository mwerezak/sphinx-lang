use core::any::Any;
use crate::runtime::Variant;
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::function::{Call, Callable};
use crate::runtime::strings::{StringValue, StringSymbol, static_symbol};
use crate::runtime::types::{Type, MetaObject};
use crate::runtime::errors::{ExecResult};


pub struct Nil;

// Nil
impl MetaObject for Nil {
    fn type_tag(&self) -> Type { Type::Nil }
    
    fn as_bool(&self) -> ExecResult<bool> { Ok(false) }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Nil => Some(Ok(true)),
            _ => None,
        }
    }
    
    fn fmt_echo(&self) -> ExecResult<StringValue> {
        Ok(StringValue::from(static_symbol!("nil")))
    }
}


// Marker type - kind of like scheme's symbols
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Marker {
    id: StringSymbol,
}

impl Marker {
    pub fn new(id: StringSymbol) -> Self {
        Self { id }
    }
}

impl MetaObject for Marker {
    fn type_tag(&self) -> Type { Type::Marker }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Marker(other) => Some(Ok(self == other)),
            _ => None,
        }
    }
    
    fn type_name(&self) -> ExecResult<StringValue> {
        Ok(StringValue::from(self.id))
    }
    
    fn fmt_echo(&self) -> ExecResult<StringValue> {
        self.type_name()
    }
}


impl<F> MetaObject for Gc<F> where F: GcTrace, Gc<F>: Callable {
    fn type_tag(&self) -> Type { Type::Function }
    
    fn invoke(&self, args: &[Variant]) -> Option<ExecResult<Call>> {
        Some(self.checked_call(args))
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Function(other) => Some(Ok(Gc::ptr_eq(self, other))),
            Variant::NativeFunction(other) => Some(Ok(Gc::ptr_eq(self, other))),
            _ => Some(Ok(false)),
        }
    }
    
    fn fmt_echo(&self) -> ExecResult<StringValue> {
        // TODO cache this in the signature struct?
        let result = format!(
            "<{} at {:#X}>", self.signature().fmt_name(), Gc::as_id(self),
        );
        
        Ok(StringValue::new_uninterned(result))
    }
}

// Native Iterators
// Similar to the Iterator trait, except the output type is fixed
// and internal mutability is required
pub trait NativeIterator: GcTrace {
    fn next(&self) -> Option<ExecResult<Variant>>;
}


// unlike UserData, the MetaObject behaviour for NativeIterator is not customizable
impl MetaObject for Gc<dyn NativeIterator> {
    fn type_tag(&self) -> Type { Type::Iterator }
    
    fn fmt_echo(&self) -> ExecResult<StringValue> {
        let result = format!(
            "<{} at {:#X}>", self.type_name()?, Gc::as_id(self)
        );
        
        Ok(StringValue::new_uninterned(result))
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Iterator(other) => Some(Ok(Gc::ptr_eq(self, other))),
            _ => Some(Ok(false)),
        }
    }
    
    fn next(&self) -> Option<ExecResult<Variant>> {
        <dyn NativeIterator>::next(&**self)
    }
}



/// Trait for custom data
pub trait UserData: Any + GcTrace + MetaObject {
    fn type_tag(&self) -> Type { Type::UserData }
}

