use crate::runtime::Variant;
use crate::runtime::gc::Gc;
use crate::runtime::strings::StringValue;
use crate::runtime::function::{Function, NativeFunction};
use crate::runtime::types::{Tuple, UserData};




/// Subset of Variant types that are references to a GC allocation
#[derive(Debug, Clone, Copy)]
pub enum GcRef {
    String(Gc<str>),
    Tuple(Tuple),
    Function(Gc<Function>),
    NativeFunction(Gc<NativeFunction>),
    UserData(Gc<dyn UserData>),
}

impl From<&GcRef> for Variant {
    fn from(gc_ref: &GcRef) -> Self {
        gc_ref.as_variant()
    }
}

impl<'a> TryFrom<&'a Variant> for GcRef {
    type Error = &'a Variant;
    fn try_from(value: &Variant) -> Result<Self, Self::Error> {
        unimplemented!()
    }
}

