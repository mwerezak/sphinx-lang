///! Support for thin pointers to dynamically sized GC data.

use core::ptr::DynMetadata;
use core::convert::Infallible;
use crate::runtime::types::{UserData, NativeIterator};


/// Because `GcBoxHeader` must not be generic this enum is used to represent the ptr metadata
/// This unfortunately means that the set of allowed metadata is closed. Luckily the most
/// important DSTs (str and [T]) use `usize` for their metadata. If additional trait objects
/// need to be GCed then new entries should be added here and From/TryInto impls provided.
#[derive(Clone, Copy)]
pub enum PtrMetadata {
    None,
    Size(usize),
    Iterator(DynMetadata<dyn NativeIterator>),
    UserData(DynMetadata<dyn UserData>),
}

pub struct PtrMetadataError;



// Sized types

impl From<()> for PtrMetadata {
    fn from(_: ()) -> Self {
        Self::None
    }
}

impl TryInto<()> for PtrMetadata {
    type Error = Infallible;
    fn try_into(self) -> Result<(), Self::Error> { Ok(()) }
}


// str and [T]

impl From<usize> for PtrMetadata {
    fn from(size: usize) -> Self {
        Self::Size(size)
    }
}

impl TryInto<usize> for PtrMetadata {
    type Error = PtrMetadataError;
    fn try_into(self) -> Result<usize, Self::Error> {
        match self {
            Self::Size(size) => Ok(size),
            _ => Err(PtrMetadataError),
        }
    }
}

// dyn NativeIterator

impl From<DynMetadata<dyn NativeIterator>> for PtrMetadata {
    fn from(meta: DynMetadata<dyn NativeIterator>) -> Self {
        Self::Iterator(meta)
    }
}

impl TryInto<DynMetadata<dyn NativeIterator>> for PtrMetadata {
    type Error = PtrMetadataError;
    fn try_into(self) -> Result<DynMetadata<dyn NativeIterator>, Self::Error> {
        match self {
            Self::Iterator(meta) => Ok(meta),
            _ => Err(PtrMetadataError),
        }
    }
}

// dyn UserData

impl From<DynMetadata<dyn UserData>> for PtrMetadata {
    fn from(meta: DynMetadata<dyn UserData>) -> Self {
        Self::UserData(meta)
    }
}

impl TryInto<DynMetadata<dyn UserData>> for PtrMetadata {
    type Error = PtrMetadataError;
    fn try_into(self) -> Result<DynMetadata<dyn UserData>, Self::Error> {
        match self {
            Self::UserData(meta) => Ok(meta),
            _ => Err(PtrMetadataError),
        }
    }
}