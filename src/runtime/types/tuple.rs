use core::fmt;
use crate::runtime::Variant;
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::types::{Type, MetaObject};
use crate::runtime::errors::ExecResult;

#[derive(Clone, Copy)]
pub enum Tuple {
    Empty,
    NonEmpty(GC<Box<[Variant]>>),  // TODO: stop using Box when DST support is stabilized
}

unsafe impl GCTrace for Box<[Variant]> {
    fn trace(&self) {
        for item in self.iter() {
            item.trace();
        }
    }
    
    fn size_hint(&self) -> usize {
        core::mem::size_of::<Variant>() * self.len()
    }
}

impl Default for Tuple {
    fn default() -> Self { Self::Empty }
}

impl From<Box<[Variant]>> for Tuple {
    fn from(items: Box<[Variant]>) -> Self {
        if items.is_empty() {
            Self::Empty
        } else {
            Self::NonEmpty(GC::new(items))
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
    
    pub fn as_gc(&self) -> Option<GC<dyn GCTrace>> {
        match self {
            Self::NonEmpty(gc_items) => Some((*gc_items).into()),
            Self::Empty => None,
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

impl fmt::Display for Tuple {
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Empty => fmt.write_str("()"),
            Self::NonEmpty(items) => {
                let (last, rest) = items.split_last().unwrap(); // will never be empty
                
                write!(fmt, "(")?;
                for item in rest.iter() {
                    write!(fmt, "{}, ", item)?;
                }
                write!(fmt, "{})", last)
            },
        }
    }
}
