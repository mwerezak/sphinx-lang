use crate::language::{IntType, FloatType};
use crate::runtime::{Variant, Gc};
use crate::runtime::module::{GlobalEnv, Access};
use crate::runtime::function::{NativeFunction, Signature, Parameter};
use crate::runtime::types::{int_from_str, float_from_str};
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
    
    let as_int = native_function!(int, env, params(value), defaults(radix = Variant::Nil) => {
        if let Some(strval) = value.as_strval() {
            return strval.with_str(|mut s| {
                // strip common prefixes
                let mut radix = match radix {
                    Variant::Nil => None,
                    radix => Some(radix.as_int()?),
                };
                
                if matches!(radix, None|Some(2)) {
                    if let Some(stripped) = s.strip_prefix("0b").or(s.strip_prefix("0B")) {
                        radix = Some(2);
                        s = stripped;
                    }
                }
                if matches!(radix, None|Some(8)) {
                    if let Some(stripped) = s.strip_prefix("0o").or(s.strip_prefix("0O")) {
                        radix = Some(8);
                        s = stripped;
                    }
                }
                if matches!(radix, None|Some(16)) {
                    if let Some(stripped) = s.strip_prefix("0x").or(s.strip_prefix("0X")) {
                        radix = Some(16);
                        s = stripped;
                    }
                }
                
                let radix = radix.unwrap_or(10);
                let int = int_from_str(s, radix)?;
                Ok(Variant::from(int))
            })
        }
        
        match value {
            Variant::Float(value) if value.is_finite() => {
                let value = value.trunc();
                if IntType::MIN as FloatType <= value && value <= IntType::MAX as FloatType {
                    Ok(Variant::from(value as IntType))
                } else {
                    Err(ErrorKind::OverflowError.into())
                }
            }
            
            _ => Ok(Variant::from(value.as_int()?))
        }
    });
    
    let as_float = native_function!(float, env, params(value) => {
        if let Some(strval) = value.as_strval() {
            return strval.with_str(
                |s| Ok(Variant::from(float_from_str(s)?))
            )
        }
        
        Ok(Variant::from(value.as_float()?))
    });
    
    // Misc
    let to_str = native_function!(str, env, params(value) => {
        if value.as_strval().is_some() {
            Ok(*value)
        } else {
            Ok(Variant::from(value.fmt_echo()?))
        }
    });
    
    let echo = native_function!(echo, env, params(value) => {
        Ok(Variant::from(value.fmt_echo()?))
    });
    
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
        fun _ = to_str;
        fun _ = echo;
        fun _ = print;
    });
    
    env
}