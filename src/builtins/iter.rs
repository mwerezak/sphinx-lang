use core::cell::RefCell;
use crate::language::IntType;
use crate::runtime::{Gc, Variant};
use crate::runtime::gc::GcTrace;
use crate::runtime::module::NamespaceEnv;
use crate::runtime::types::UserIterator;
use crate::runtime::iter::IterState;
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


struct ZipIter {
    iters: RefCell<Box<[IterState]>>
}

unsafe impl GcTrace for ZipIter {
    fn trace(&self) {
        self.iters.borrow()
            .iter().for_each(IterState::trace);
    }
}

impl ZipIter {
    fn new<'a>(iterables: impl Iterator<Item=&'a Variant>) -> ExecResult<Self> {
        let iters = iterables.map(|seq| seq.iter_init())
            .collect::<Result<Vec<IterState>,_>>()?
            .into_boxed_slice();
        
        Ok(Self { iters: RefCell::new(iters) })
    }
}

impl UserIterator for ZipIter {
    fn next_state(&self, state: Option<&Variant>) -> ExecResult<Variant> {
        let mut iters = self.iters.borrow_mut();
        
        // already initialized all of the iters in new()
        if state.is_some() {
            iters.iter_mut().try_for_each(IterState::advance)?;
        }
        
        for iter in iters.iter() {
            if !iter.has_value()? {
                return Ok(Variant::BoolFalse);
            }
        }
        Ok(Variant::BoolTrue)
    }
    
    fn get_item(&self, _: &Variant) -> ExecResult<Variant> {
        let mut item = Vec::new();
        for iter in self.iters.borrow().iter() {
            item.push(iter.get_value()?);
        }
        Ok(Variant::from(item.into_boxed_slice()))
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
    let zip = native_function!(zip, env, variadic(iterables) => {
        let iter = Box::new(ZipIter::new(iterables.iter())?);
        Ok(Variant::Iterator(Gc::from_box(iter)))
    });
    
    namespace_insert!(env.borrow_mut(), {
        fun _ = range;
        fun _ = zip;
    });
}
