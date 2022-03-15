///! Metamethod definitions for builtin types

use crate::runtime::Variant;
use crate::runtime::types::Metatable;
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


// Note: most ops are short-circuited for numeric types in runtime::ops

lazy_static! {
    pub static ref METATABLE_DEFAULT: Metatable = {
        Metatable::default()
    };
}


// Strings

// fn string_add(lhs: &Variant, rhs: &Variant) -> ExecResult<Option<Variant>> {
    
// }