use core::fmt::{self, Write};
use crate::runtime::Variant;
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::strings::{StringValue, static_symbol};
use crate::runtime::types::{Type, MetaObject, IterState, UserIterator};
use crate::runtime::errors::{ExecResult, RuntimeError};

#[derive(Clone, Copy)]
pub enum Tuple {
    Empty,
    NonEmpty(Gc<[Variant]>),
}

impl Default for Tuple {
    fn default() -> Self { Self::Empty }
}

impl From<Box<[Variant]>> for Tuple {
    fn from(items: Box<[Variant]>) -> Self {
        if items.is_empty() {
            Self::Empty
        } else {
            Self::NonEmpty(Gc::from_box(items))
        }
    }
}

impl AsRef<[Variant]> for Tuple {
    fn as_ref(&self) -> &[Variant] {
        match self {
            Self::Empty => &[] as &[Variant],
            Self::NonEmpty(items) => &**items,
        }
    }
}

impl Tuple {
    pub fn trace(&self) {
        if let Self::NonEmpty(gc_items) = self {
            gc_items.mark_trace()
        }
    }
    
    pub fn len(&self) -> usize {
        match self {
            Self::Empty => 0,
            Self::NonEmpty(items) => items.len(),
        }
    }
    
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Empty => true,
            Self::NonEmpty(..) => false,
        }
    }
}

impl MetaObject for Tuple {
    fn type_tag(&self) -> Type { Type::Tuple }
    
    fn len(&self) -> Option<ExecResult<usize>> {
        Some(Ok(Tuple::len(self)))
    }
    
    fn iter_init(&self) -> Option<ExecResult<IterState>> {
        let iter: Box<dyn UserIterator> = Box::new(TupleIter(*self));
        let iter = Gc::from_box(iter);
        iter.iter_init()
    }
    
    fn fmt_repr(&self) -> ExecResult<StringValue> {
        match self {
            Self::Empty => Ok(StringValue::from(static_symbol!("()"))),
            
            Self::NonEmpty(items) => {
                let (first, rest) = items.split_first().unwrap(); // should never be empty
                
                let mut buf = String::new();
                
                write!(&mut buf, "({}", first.fmt_repr()?)
                    .map_err(|err| RuntimeError::other(err.to_string()))?;
                
                for item in rest.iter() {
                    write!(&mut buf, ", {}", item.fmt_repr()?)
                        .map_err(|err| RuntimeError::other(err.to_string()))?;
                }
                buf.push(')');

                
                Ok(StringValue::new_maybe_interned(buf))
            }
        }
    }
}

impl fmt::Debug for Tuple {
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut tuple = fmt.debug_tuple("");
        for item in self.as_ref().iter() {
            tuple.field(item);
        }
        tuple.finish()
    }
}

// Tuple Iterator
#[derive(Debug)]
struct TupleIter(Tuple);

unsafe impl GcTrace for TupleIter {
    fn trace(&self) {
        self.0.trace()
    }
}

impl UserIterator for TupleIter {
    fn get_item(&self, state: &Variant) -> ExecResult<Variant> {
        let idx = usize::try_from(state.as_int()?)
            .map_err(|_| RuntimeError::invalid_value("invalid state"))?;
        
        let items = self.0.as_ref();
        Ok(items[idx])
    }
    
    fn next_state(&self, state: Option<&Variant>) -> ExecResult<Variant> {
        let next = match state {
            Some(state) => state.as_int()?
                .checked_add(1)
                .ok_or(RuntimeError::overflow_error())?,
            
            None => 0,
        };
        
        let next_idx = usize::try_from(next)
            .map_err(|_| RuntimeError::invalid_value("invalid state"))?;
        
        if next_idx >= self.0.len() {
            return Ok(Variant::Nil)
        }
        Ok(Variant::from(next))
    }
}