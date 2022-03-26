use std::fmt;
use crate::codegen::ChunkID;
use crate::runtime::Variant;
use crate::runtime::module::ModuleID;
use crate::runtime::errors::ExecResult;

pub mod operator;
pub mod metatable;
pub mod primitive;
pub mod function;


/// Call directive
pub enum Call {
    Chunk(ModuleID, ChunkID),    // the module & chunk to call into
    Native(ExecResult<Variant>), // eagerly computed result
}


// Type tag for Sphinx's "primitive" types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Boolean,
    Integer,
    Float,
    String,
    Tuple,
    Function,
    Metatable,
    Object,
}


impl fmt::Display for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Nil => "nil",
            Self::Boolean => "bool",
            Self::Integer => "int",
            Self::Float => "float",
            Self::String => "string",
            Self::Tuple => "tuple",
            Self::Function => "function",
            Self::Metatable => "metatable",
            
            // note, when looking up the type of an object value 
            // the object's metatable should be used to generate the type name
            Self::Object => "object",
        };
        fmt.write_str(name)
    }
}