/// Data structures for constant values that are compiled with chunks

use core::mem;
use string_interner::Symbol;
use crate::language::{IntType, FloatType, InternSymbol};


pub type ConstID = u16;
pub type StringID = usize;


// Constants

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constant {
    Integer(IntType),
    Float([u8; mem::size_of::<FloatType>()]),  // we might store redundant floats, that's fine
    String(StringID),
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

impl Constant {
    pub fn try_into_int(self) -> Option<IntType> {
        match self {
            Constant::Integer(value) => Some(value),
            _ => None,
        }
    }
    
    pub fn try_into_float(self) -> Option<FloatType> {
        match self {
            Constant::Float(bytes) => Some(FloatType::from_le_bytes(bytes)),
            _ => None,
        }
    }
    
    pub fn try_into_string_id(self) -> Option<StringID> {
        match self {
            Constant::String(string_id) => Some(string_id),
            _ => None,
        }
    }
}

