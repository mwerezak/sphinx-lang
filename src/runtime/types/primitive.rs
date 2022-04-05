///! Metamethod definitions for builtin types

use crate::runtime::Variant;
use crate::runtime::errors::ExecResult;

use crate::runtime::types::metatable::*;

// Note: most ops are short-circuited for numeric types in runtime::ops

lazy_static! {
    pub static ref METATABLE_DEFAULT: Metatable = {
        Metatable::default()
    };
    
    pub static ref METATABLE_STRING: Metatable = {
        let mut metatable = Metatable::default();
        metatable.set_binary(BinaryTag::Add, string_add);
        metatable
    };
}

// Strings

fn string_add(lhs: &Variant, rhs: &Variant) -> ExecResult<Option<Variant>> {
    if let (Variant::String(a), Variant::String(b)) = (lhs, rhs) {
        Ok(Some(Variant::String(a.concat(b))))
    } else {
        Ok(None)
    }
}