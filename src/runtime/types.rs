use std::fmt;
use once_cell::sync::Lazy;

use crate::runtime::Variant;
use crate::runtime::errors::ExecResult;

pub mod operator;
pub mod metatable;

pub use metatable::Metatable;
use metatable::*;

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



pub static METATABLE_DEFAULT: Lazy<Metatable> = Lazy::new(Metatable::default);

pub static METATABLE_STRING: Lazy<Metatable> = Lazy::new(|| {
    
    fn string_add(lhs: &Variant, rhs: &Variant) -> ExecResult<Option<Variant>> {
        if let (Variant::String(a), Variant::String(b)) = (lhs, rhs) {
            Ok(Some(Variant::String(a.concat(b))))
        } else {
            Ok(None)
        }
    }
    
    let mut metatable = Metatable::default();
    metatable.set_binary(BinaryTag::Add, string_add);
    metatable
});
