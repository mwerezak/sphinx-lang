/// Data structures for constant values that are compiled with chunks

use std::mem;
use string_interner::Symbol;
use crate::language::{IntType, FloatType, InternSymbol};
use crate::parser::lvalue::DeclType;
use crate::codegen::chunk::ChunkID;


pub type ConstID = u16;
pub type StringID = usize;
pub type FunctionID = usize;


// Constants

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constant {
    Integer(IntType),
    Float([u8; mem::size_of::<FloatType>()]),  // we might store redundant floats, that's fine
    String(StringID),
    Function(ChunkID, FunctionID),  // signatures are stored separately to save memory - referenced by "FunctionID"
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



#[derive(Clone, Debug)]
pub struct UnloadedSignature {
    pub name: Option<ConstID>,
    pub required: Box<[UnloadedParam]>,
    pub default: Box<[UnloadedParam]>,
    pub variadic: Option<UnloadedParam>,
}

#[derive(Clone, Debug)]
pub struct UnloadedParam {
    pub name: ConstID,
    pub decl: DeclType,
}
