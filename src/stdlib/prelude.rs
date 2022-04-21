use crate::language::IntType;
use crate::runtime::{Variant, Gc};
use crate::runtime::module::{GlobalEnv, Access};
use crate::runtime::function::{NativeFunction, Signature, Parameter};
use crate::runtime::vm::VirtualMachine;
use crate::runtime::errors::{ExecResult, ErrorKind};


/// Create an Env containing the core builtins
pub fn create_prelude() -> Gc<GlobalEnv> {
    let env = GlobalEnv::new();
    
    // Metamethods
    
    // Get the length of a container using the `__len` metamethod.
    let len = native_function!(len, env, params(value) => {
        let len = value.len()?;
        match IntType::try_from(len) {
            Ok(len) => Ok(Variant::from(len)),
            Err(..) => Err(ErrorKind::OverflowError.into()),
        }
    });
    
    // primitive type constructors
    
    let as_bool = native_function!(bool, env, params(value) => {
        Ok(Variant::from(value.as_bool()?))
    });
    
    let as_bits = native_function!(bitfield, env, params(value) => {
        Ok(Variant::from(value.as_bits()?))
    });
    
    let as_int = native_function!(int, env, params(value) => {
        Ok(Variant::from(value.as_int()?))
    });
    
    let as_float = native_function!(float, env, params(value) => {
        Ok(Variant::from(value.as_float()?))
    });
    
    // Misc
    
    let print = native_function!(print, env, variadic(values)  => {
        if let Some((first, rest)) = values.split_first() {
            print!("{}", first);
            for value in rest.iter() {
                print!(" ");
                print!("{}", value);
            }
        }
        println!();
        
        Ok(Variant::Nil)
    });
    
    // Produces a tuple of the global names in the current call frame
    // TODO return an object or a namespace instead?
    let globals = native_function!(globals, env, vm(vm)  => {
        let global_env = vm.frame().module().globals();
        let names = global_env.borrow().names()
            .map(|name| Variant::from(*name))
            .collect::<Vec<Variant>>()
            .into_boxed_slice();
            
        Ok(Variant::from(names))
    });
    
    namespace!(env.borrow_mut(), {
        fun _ = len;
        
        fun _ = as_bool;
        fun _ = as_bits;
        fun _ = as_int;
        fun _ = as_float;
        
        fun _ = globals;
        fun _ = print;
    });
    
    env
}