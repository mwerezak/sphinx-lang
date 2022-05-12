use crate::runtime::Variant;
use crate::runtime::gc::GcTrace;
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
    iter: Variant,
    state: Variant,
}

unsafe impl GcTrace for IterState {
    fn trace(&self) {
        self.iter.trace();
        self.state.trace();
    }
}

impl IterState {
    pub fn new(iter: Variant, state: Variant) -> Self {
        Self { iter, state }
    }
    
    #[inline]
    pub fn get_iter(&self) -> &Variant { &self.iter }
    
    #[inline]
    pub fn get_state(&self) -> &Variant { &self.state }
    
    // Helpers
    #[inline]
    pub fn has_value(&self) -> ExecResult<bool> {
        self.state.as_bool()
    }
    
    #[inline]
    pub fn get_value(&self) -> ExecResult<Variant> {
        self.iter.iter_get(&self.state)
    }
    
    #[inline]
    pub fn next_state(&self) -> ExecResult<Variant> {
        self.iter.iter_next(&self.state)
    }
    
    #[inline]
    pub fn next(&self) -> ExecResult<IterState> {
        Ok(Self {
            iter: self.iter,
            state: self.next_state()?,
        })
    }
    
    // go to the next state *in place*
    #[inline]
    pub fn advance(&mut self) -> ExecResult<()> {
        self.state = self.iter.iter_next(&self.state)?;
        Ok(())
    }
}

impl IntoIterator for IterState {
    type Item = ExecResult<Variant>;
    type IntoIter = Iter;
    fn into_iter(self) -> Self::IntoIter {
        Iter(self)
    }
}


/// wrapper for IterState that allows it to be used as a Rust Iterator
pub struct Iter(IterState);

impl Iter {
    fn next_value(&mut self) -> ExecResult<Option<Variant>> {
        if !self.0.has_value()? {
            return Ok(None)
        }
        
        let next = self.0.get_value()?;
        self.0.advance()?;
        Ok(Some(next))
    }
}

impl Iterator for Iter {
    type Item = ExecResult<Variant>;
    
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_value() {
            Ok(None) => None,
            Ok(Some(value)) => Some(Ok(value)),
            Err(error) => Some(Err(error)),
        }
    }
}

