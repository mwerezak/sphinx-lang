use crate::runtime::Variant;
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::strings::StringValue;
use crate::runtime::types::{Type, MetaObject};
use crate::runtime::errors::{ExecResult};


/*
    The iterator protocol in a nutshell:
    
    Certain values are "iterables" and can be iterated. For example: containers, strings, streams, etc.
    These values support the `iter_init()` method which produces an "iterator state".
    
    An iterator state is just a pair: an "iterator" and a state. The state can be any value, 
    however false values are interpeted as indicating that the iterator is exhausted.
    The iterator is a value that supports the `iter_get()` and `iter_next()` methods. 
    
    Both of these methods take the state value as their argument.
    
    `iter_next()` takes a state value and produces the next state value.
    
    `iter_get()` gets the item for a given state.
    This method is expected to succeed when called for the first time with the current state (assuming
    that the state is true), but is not required to succeed when called with the same state subsequent 
    times or when called with a past state.
    
    Note: it is not required for an iterator to actually use the state argument.
    The alternative is interior mutability: the iterator can just mutate itself when `iter_next()` is called.
    Such iterators are called "stateful" and the state is only needed to signal the end of iteration.
    
*/

pub struct IterState {
    pub iter: Variant,
    pub state: Variant,
}

/// Similar use case as UserData but a bit more limited in scope
pub trait UserIterator: GcTrace {
    fn next_state(&self, state: Option<&Variant>) -> ExecResult<Variant>;
    fn get_item(&self, state: &Variant) -> ExecResult<Variant>;
}


// unlike UserData, the MetaObject impl for UserIterator is not customizable
impl MetaObject for Gc<dyn UserIterator> {
    fn type_tag(&self) -> Type { Type::Iterator }
    
    fn fmt_repr(&self) -> ExecResult<StringValue> {
        let result = format!(
            "<{} at {:#X}>", self.type_name()?, Gc::as_id(self)
        );
        
        Ok(StringValue::new_uninterned(result))
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Iterator(other) => Some(Ok(Gc::ptr_eq(self, other))),
            _ => Some(Ok(false)),
        }
    }
    
    fn iter_get(&self, state: &Variant) -> Option<ExecResult<Variant>> {
        Some(self.get_item(state))
    }
    
    fn iter_next(&self, state: &Variant) -> Option<ExecResult<Variant>> {
        Some(self.next_state(Some(state)))
    }
    
    fn iter_init(&self) -> Option<ExecResult<IterState>> {
        let state = match self.next_state(None) {
            Ok(state) => state,
            Err(error) => return Some(Err(error)),
        };
        
        let iter = IterState {
            iter: Variant::Iterator(*self),
            state,
        };
        
        Some(Ok(iter))
    }
}
