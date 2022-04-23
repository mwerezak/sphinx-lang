use core::cell::Cell;
use core::ptr::NonNull;
use crate::runtime::gc::GC_STATE;
use crate::runtime::gc::data::{GcTrace, GcBox, WeakCell};

#[derive(Debug)]
pub(super) struct GcWeakCell<T> where T: GcTrace + ?Sized + 'static {
    ptr: Cell<Option<NonNull<GcBox<T>>>>,
}

unsafe impl<T> GcTrace for GcWeakCell<T> where T: GcTrace + ?Sized {
    fn trace(&self) { }
}

impl<T> GcWeakCell<T> where T: GcTrace + ?Sized {
    pub(super) fn new(gcbox: NonNull<GcBox<T>>) -> Self {
        Self {
            ptr: Cell::new(Some(gcbox))
        }
    }
    
    pub(super) fn get(&self) -> Option<NonNull<GcBox<T>>> {
        self.ptr.get()
    }
    
    pub(super) fn invalidate(&self) {
        if let Some(weak_ptr) = self.ptr.get() {
            log::debug!("{:#X} invalidate weak reference", weak_ptr.as_ptr() as *mut () as usize);
        }
        
        self.ptr.set(None)
    }
}

impl<T> Drop for GcWeakCell<T> where T: GcTrace + ?Sized {
    fn drop(&mut self) {
        if let Some(mut weak_ptr) = self.ptr.get() {
            log::debug!("{:#X} weak reference dropped", weak_ptr.as_ptr() as *mut () as usize);
            
            unsafe { weak_ptr.as_mut() }.clear_weak();
        }
    }
}

impl<T> WeakCell for GcWeakCell<T> where T: GcTrace + ?Sized {
    fn invalidate(&self) {
        GcWeakCell::invalidate(self)
    }
}


impl<T> GcBox<T> where T: GcTrace + ?Sized {
    /// Get a pointer to the weak reference allocation for this `GcBox<T>`
    /// if it already exists, otherwise allocate a new one
    pub(super) fn get_or_make_weak(&mut self) -> NonNull<GcBox<GcWeakCell<T>>> {
        if let Some(weak_ptr) = self.header().weak() {
            weak_ptr.cast()
        } else {
            let self_ptr = unsafe { NonNull::new_unchecked(self) };
            let weak_cell = GcWeakCell::new(self_ptr);
            let gcbox_weak = GcBox::new(weak_cell);
            
            // update the header weakref data
            let dyn_ptr = gcbox_weak.as_ptr() as *mut GcBox<dyn WeakCell>;
            self.header_mut().set_weak(unsafe { Some(NonNull::new_unchecked(dyn_ptr)) });
            
            // insert the new GcBox<GcWeakCell<T>> into GC tracking
            GC_STATE.with(|gc| gc.borrow_mut().insert(gcbox_weak));
            
            gcbox_weak
        }
    }
    
    fn clear_weak(&mut self) {
        self.header_mut().set_weak(None);
    }
}
