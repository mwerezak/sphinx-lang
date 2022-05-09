use core::cell::RefCell;
use crate::language::IntType;
use crate::runtime::{Gc, Variant};
use crate::runtime::gc::GcTrace;
use crate::runtime::module::NamespaceEnv;
use crate::runtime::types::{UserIterator, IterState};
use crate::runtime::errors::{RuntimeError, ExecResult};


struct RangeIter {
    start: IntType,
    stop: IntType,
    step: IntType,
}

unsafe impl GcTrace for RangeIter {
    fn trace(&self) { }
}

impl UserIterator for RangeIter {
    fn get_item(&self, state: &Variant) -> ExecResult<Variant> {
        Ok(*state)
    }
    
    fn next_state(&self, state: Option<&Variant>) -> ExecResult<Variant> {
        let next = match state {
            Some(state) => state.as_int()?
                .checked_add(self.step)
                .ok_or(RuntimeError::overflow_error())?,
            
            None => self.start,
        };
        
        if self.step.is_positive() {
            if next >= self.stop {
                return Ok(Variant::Nil);
            }
        } else {
            if next <= self.stop {
                return Ok(Variant::Nil);
            }
        }
        
        Ok(Variant::from(next))
    }
}

pub fn create_iter_builtins(env: Gc<NamespaceEnv>) {
    
    // produces an iterable that yields a succession of integers controlled by start, stop, and step values.
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
            return Err(RuntimeError::invalid_value("step cannot be zero"));
        }
        
        let range_iter = Box::new(RangeIter {
            start: start_value,
            stop: stop_value,
            step: step_value,
        });
        Ok(Variant::Iterator(Gc::from_box(range_iter)))
    });
    
    // yields tuples containg an element from each iterable until the first iterable is exhausted.
    // let zip = native_function!(zip, env, variadic(iterables) => {
        
    // })
    
    namespace_insert!(env.borrow_mut(), {
        fun _ = range;
    });
}
