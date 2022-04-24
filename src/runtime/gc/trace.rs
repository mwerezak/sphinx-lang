use core::cell::{Cell, RefCell};


/// Trait required for all GC'd data.
/// Unsafe because if the `trace()` implementation fails to call `Gc::mark_trace()` 
/// and `GcWeak::mark_trace()` on all of the `Gc` and `GcWeak` pointers that it can reach,
/// the GC will free memory that is still in use.
/// SAFETY: If the receiver also impls `Drop`, the `drop()` impl must not deref any `Gc` or `GcWeak` pointers
pub unsafe trait GcTrace {
    
    /// SAFETY: Must call `Gc::mark_trace()` on every reachable Gc handle
    fn trace(&self);
    
    /// If the GcTrace owns any allocations, this should return the extra allocated size.
    /// If the allocation can change size, like a Vec<T>, then don't include it in the 
    /// size hint, or return a const estimate of the average size.
    #[inline]
    fn size_hint(&self) -> usize { 0 }
    
    fn cleanup(&self) { }
}

// Arrays
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

// Cells
unsafe impl<T> GcTrace for Cell<T> where T: GcTrace + Copy {
    fn trace(&self) {
        self.get().trace()
    }
    
    fn size_hint(&self) -> usize {
        self.get().size_hint()
    }
}

unsafe impl<T> GcTrace for RefCell<T> where T: GcTrace {
    fn trace(&self) {
        self.borrow().trace()
    }
    
    fn size_hint(&self) -> usize {
        self.borrow().size_hint()
    }
}