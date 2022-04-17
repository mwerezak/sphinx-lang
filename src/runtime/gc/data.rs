use core::fmt;
use core::mem;
use core::ptr::{self, NonNull};
use core::cell::{Cell, RefCell};

/// TODO store GCBoxes in linear chunks instead of individual linked nodes
pub struct GCBox<T> where T: GCTrace + ?Sized + 'static {
    next: Option<NonNull<GCBox<dyn GCTrace>>>,
    marked: bool,
    data: T,
}

// constructor for sized types
impl<T> GCBox<T> where T: GCTrace {
    pub fn new(data: T) -> NonNull<GCBox<T>> {
        let gcbox = Box::new(GCBox {
            next: None,
            marked: false,
            data,
        });
        
        unsafe { NonNull::new_unchecked(Box::into_raw(gcbox)) }
    }
}

// for arrays
// impl<T> GCBox<[T]> where T: GCTrace {
    
// }

impl<T> GCBox<T> where T: GCTrace + ?Sized {
    #[inline]
    pub fn value(&self) -> &T { &self.data }
    
    #[inline]
    pub fn next(&self) -> Option<NonNull<GCBox<dyn GCTrace>>> {
        self.next
    }
    
    #[inline]
    pub fn set_next(&mut self, next: Option<NonNull<GCBox<dyn GCTrace>>>) {
        self.next = next
    }
    
    #[inline]
    pub fn size(&self) -> usize {
        mem::size_of_val(self) + self.value().size_hint()
    }
    
    #[inline]
    pub fn ptr_eq(&self, other: &GCBox<T>) -> bool {
        // in case T is a trait object, work around for <https://github.com/rust-lang/rust/issues/46139>
        ptr::eq(&self.marked, &other.marked)
    }
    
    #[inline]
    pub fn is_marked(&self) -> bool {
        self.marked
    }
    
    #[inline]
    pub fn mark_trace(&mut self) {
        if !self.marked {
            self.marked = true;
            self.data.trace();
        }
    }
    
    #[inline]
    pub fn clear_mark(&mut self) {
        self.marked = false
    }
}


/// Unsafe because if the GCTrace::trace() implementation fails to mark any GC handles that it can reach, 
/// the GC will not be able to mark them and will free memory that is still in use.
/// SAFETY: Must not impl Drop
pub unsafe trait GCTrace {
    
    /// SAFETY: Must call `GC::mark_trace()` on every reachable GC handle
    fn trace(&self);
    
    /// If the GCTrace owns any allocations, this should return the extra allocated size.
    /// If the allocation can change size, like a Vec<T>, then don't include it in the 
    /// size hint, or return a const estimate of the average size.
    #[inline]
    fn size_hint(&self) -> usize { 0 }
}