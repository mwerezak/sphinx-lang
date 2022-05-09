use crate::language::{IntType, FloatType};
use crate::runtime::Gc;
use crate::runtime::module::NamespaceEnv;
use crate::runtime::types::{int_from_str, float_from_str};
use crate::runtime::errors::RuntimeError;


// primitive type constructors
pub fn create_primitive_ctors(env: Gc<NamespaceEnv>) {
    
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
                    Err(RuntimeError::overflow_error())
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
    
    // convert a value into a string
    let as_str = native_function!(str, env, params(value) => {
        Ok(Variant::from(value.fmt_str()?))
    });
    
    // marker type constructor
    // let marker = native_function!(marker, env, params(marker) => {
    //     let symbol = marker.as_strval()
    //         .ok_or(ErrorKind::StaticMessage("marker discriminant must be a string"))?
    //         .as_intern();
            
    //     Ok(Variant::marker(symbol))
    // });
    
    namespace_insert!(env.borrow_mut(), {
        fun _ = as_bool;
        fun _ = as_bits;
        fun _ = as_int;
        fun _ = as_float;
        fun _ = as_str;
    });
}


pub fn create_metamethod_builtins(env: Gc<NamespaceEnv>) {
    
    // Get the length of a container using the `__len` metamethod.
    let len = native_function!(len, env, params(value) => {
        let len = value.len()?;
        match IntType::try_from(len) {
            Ok(len) => Ok(Variant::from(len)),
            Err(..) => Err(RuntimeError::overflow_error()),
        }
    });
    
    // produces a tuple (item, next_state) for a given iterator state
    let next = native_function!(next, env, params(value), defaults(state = Variant::Nil) => {
        let result = vec![ 
            value.iter_get(state)?,
            value.iter_next(state)?,
        ];
        
        Ok(Variant::from(result.into_boxed_slice()))
    });
    
    // produces a tuple (iterator, init_state)
    let iter = native_function!(iter, env, params(value) => {
        let iter = value.iter_init()?;
        
        let result = vec![
            *iter.iter(),
            *iter.state(),
        ];
        
        Ok(Variant::from(result.into_boxed_slice()))
    });
    
    // Misc

    
    namespace_insert!(env.borrow_mut(), {
        fun _ = len;
        fun _ = iter;
        fun _ = next;
    });
}