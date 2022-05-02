use crate::language::{IntType, FloatType};
use crate::runtime::Gc;
use crate::runtime::module::{GlobalEnv};
use crate::runtime::types::{int_from_str, float_from_str};
use crate::runtime::errors::{ErrorKind, ExecResult};


use crate::runtime::Variant;
use crate::runtime::types::UserIterator;
use crate::runtime::gc::GcTrace;

#[derive(Debug)]
struct RangeIter {
    start: IntType,
    stop: IntType,
    step: IntType,
}

unsafe impl GcTrace for RangeIter {
    fn trace(&self) { }
}

impl RangeIter {
    fn new(start: IntType, stop: IntType, step: IntType) -> Self {
        Self { start, stop, step }
    }
}

impl UserIterator for RangeIter {
    fn get_item(&self, state: &Variant) -> ExecResult<Variant> {
        Ok(*state)
    }
    
    fn next_state(&self, state: Option<&Variant>) -> ExecResult<Option<Variant>> {
        let next = match state {
            Some(state) => state.as_int()?
                .checked_add(self.step)
                .ok_or(ErrorKind::OverflowError)?,
            
            None => self.start,
        };
        
        if self.step.is_positive() {
            if next >= self.stop {
                return Ok(None);
            }
        } else {
            if next <= self.stop {
                return Ok(None);
            }
        }
        
        Ok(Some(Variant::from(next)))
    }
}


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
            iter.iter,
            iter.state,
        ];
        
        Ok(Variant::from(result.into_boxed_slice()))
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
    
    // marker type constructor
    let marker = native_function!(marker, env, params(marker) => {
        let symbol = marker.as_strval()
            .ok_or(ErrorKind::StaticMessage("marker discriminant must be a string"))?
            .as_intern();
            
        Ok(Variant::marker(symbol))
    });
    
    // Misc
    let to_str = native_function!(str, env, params(value) => {
        Ok(Variant::from(value.fmt_str()?))
    });
    
    let echo = native_function!(echo, env, params(value) => {
        Ok(Variant::from(value.fmt_echo()?))
    });
    
    let print = native_function!(print, env, variadic(values) => {
        if let Some((first, rest)) = values.split_first() {
            print!("{}", first.fmt_str()?);
            for value in rest.iter() {
                print!(" ");
                print!("{}", value.fmt_str()?);
            }
        }
        println!();
        
        Ok(Variant::Nil)
    });
    
    let range = native_function!(range, env, params(start), defaults(stop = Variant::Nil, step = 1) => {
        let start_value;
        let stop_value;
        if stop.is_nil() {
            start_value = 0;
            stop_value = start.as_int()?;
        } else {
            start_value = start.as_int()?;
            stop_value = stop.as_int()?;
        }
        
        let step_value = step.as_int()?;
        if step_value == 0 {
            return Err(ErrorKind::Message("step cannot be zero".to_string()).into());
        }
        
        let range_iter = Box::new(RangeIter::new(start_value, stop_value, step_value));
        Ok(Variant::Iterator(Gc::from_box(range_iter)))
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
        let StopIteration = Variant::stop_iteration();
        
        fun _ = len;
        fun _ = iter;
        fun _ = next;
        
        fun _ = as_bool;
        fun _ = as_bits;
        fun _ = as_int;
        fun _ = as_float;
        
        fun _ = range;
        
        fun _ = globals;
        fun _ = marker;
        fun _ = to_str;
        fun _ = echo;
        fun _ = print;
    });
    
    env
}