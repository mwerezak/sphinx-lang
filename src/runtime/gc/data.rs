use core::fmt;
use core::mem;
use core::ptr::{self, NonNull};
use core::cell::{Cell, RefCell};

/// TODO store GcBoxes in linear chunks instead of individual linked nodes
pub struct GcBox<T> where T: GcTrace + ?Sized + 'static {
    next: Option<NonNull<GcBox<dyn GcTrace>>>,
    marked: bool,
    data: T,
}

// constructor for sized types
impl<T> GcBox<T> where T: GcTrace {
    pub fn new(data: T) -> NonNull<GcBox<T>> {
        let gcbox = Box::new(GcBox {
            next: None,
            marked: false,
            data,
        });
        
        unsafe { NonNull::new_unchecked(Box::into_raw(gcbox)) }
    }
}

// For arrays
// impl<T> GcBox<[T]> where T: GcTrace {
    
// }

impl<T> GcBox<T> where T: GcTrace + ?Sized {
    #[inline]
    pub fn value(&self) -> &T { &self.data }
    
    #[inline]
    pub fn next(&self) -> Option<NonNull<GcBox<dyn GcTrace>>> {
        self.next
    }
    
    #[inline]
    pub fn set_next(&mut self, next: Option<NonNull<GcBox<dyn GcTrace>>>) {
        self.next = next
    }
    
    #[inline]
    pub fn size(&self) -> usize {
        mem::size_of_val(self) + self.value().size_hint()
    }
    
    #[inline]
    pub fn ptr_eq(&self, other: &GcBox<T>) -> bool {
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


/// Unsafe because if the GcTrace::trace() implementation fails to mark any Gc handles that it can reach, 
/// the Gc will not be able to mark them and will free memory that is still in use.
/// SAFETY: Must not impl Drop
pub unsafe trait GcTrace {
    
    /// SAFETY: Must call `Gc::mark_trace()` on every reachable Gc handle
    fn trace(&self);
    
    /// If the GcTrace owns any allocations, this should return the extra allocated size.
    /// If the allocation can change size, like a Vec<T>, then don't include it in the 
    /// size hint, or return a const estimate of the average size.
    #[inline]
    fn size_hint(&self) -> usize { 0 }
}


// arrays
unsafe impl<T> GcTrace for [T] where T: GcTrace {
    fn trace(&self) {
        for item in self.iter() {
            item.trace()
        }
    }
    
    fn size_hint(&self) -> usize {
        self.iter().map(GcTrace::size_hint).sum()
    }
}
