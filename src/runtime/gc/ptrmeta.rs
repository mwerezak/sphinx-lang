///! Support for thin pointers to dynamically sized GC data.

use core::ptr::DynMetadata;
use crate::runtime::types::UserData;


/// Because `GcBoxHeader` must not be generic the set of allowed 
/// pointer metadata is closed and defined by this enum.
#[derive(Clone, Copy)]
pub enum PtrMetadata {
    None,
    Size(usize),
    UserData(DynMetadata<dyn UserData>)
}

// Sized types

impl From<()> for PtrMetadata {
    fn from(_: ()) -> Self {
        Self::None
    }
}

impl TryInto<()> for PtrMetadata {
    type Error = ();
    fn try_into(self) -> Result<(), Self::Error> { Ok(()) }
}


// str and [T]

impl From<usize> for PtrMetadata {
    fn from(size: usize) -> Self {
        Self::Size(size)
    }
}

impl TryInto<usize> for PtrMetadata {
    type Error = ();
    fn try_into(self) -> Result<usize, Self::Error> {
        match self {
            Self::Size(size) => Ok(size),
            _ => Err(()),
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
    type Error = ();
    fn try_into(self) -> Result<DynMetadata<dyn UserData>, Self::Error> {
        match self {
            Self::UserData(meta) => Ok(meta),
            _ => Err(()),
        }
    }
}