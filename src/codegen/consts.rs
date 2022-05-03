/// Data structures for constant values that are compiled with chunks

use core::mem;
use string_interner::Symbol;
use crate::language::{IntType, FloatType, InternSymbol};
use crate::runtime::errors::ErrorKind;

pub type ConstID = u16;
pub type StringID = usize;


// Constants

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constant {
    Integer(IntType),
    Float([u8; mem::size_of::<FloatType>()]),  // we might store redundant floats, that's fine
    String(StringID),
    Error { error: ErrorKind, message: StringID },
}

impl From<IntType> for Constant {
    fn from(value: IntType) -> Self { Self::Integer(value) }
}

impl From<FloatType> for Constant {
    fn from(value: FloatType) -> Self { Self::Float(value.to_le_bytes()) }
}

impl From<InternSymbol> for Constant {
    fn from(symbol: InternSymbol) -> Self { Self::String(symbol.to_usize()) }
}
