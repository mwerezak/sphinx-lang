use crate::language::IntType;
use crate::runtime::{Variant, Gc};
use crate::runtime::module::{GlobalEnv, Access};
use crate::runtime::function::{NativeFunction, Signature, Parameter};
use crate::runtime::errors::{ExecResult, ErrorKind};


/// Create an Env containing the core builtins
pub fn create_prelude() -> Gc<GlobalEnv> {
    let env = GlobalEnv::new();
    
    // Get the length of a container using the `__len` metamethod.
    let len = native_function!(len, env, _, params(obj) => {
        let len = obj.len()?;
        match IntType::try_from(len) {
            Ok(len) => Ok(Variant::from(len)),
            Err(..) => Err(ErrorKind::OverflowError.into()),
        }
    });
    
    namespace!(env.borrow_mut(), {
        fun _ = len;
    });
    
    env
}