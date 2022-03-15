///! Metamethod definitions for builtin types

use crate::runtime::{Variant, STRING_TABLE};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};

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
        return STRING_TABLE.with(|string_table| {
            let mut buf = String::new();
            buf.push_str(&string_table.resolve(a));
            buf.push_str(&string_table.resolve(b));
            Ok(Some(Variant::from(buf.as_str())))
        })
    }
    Ok(None)
}