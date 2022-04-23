use core::fmt;
use core::ops::Deref;
use core::borrow::Borrow;
use core::ptr::{self, NonNull, Pointee};
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use std::rc::Rc;

use crate::runtime::gc::{GC_STATE, deref_safe};
use crate::runtime::gc::data::{GcBox, GcTrace};
use crate::runtime::gc::weak::GcWeakCell;


pub struct Gc<T> where T: GcTrace + ?Sized + 'static {
    ptr: NonNull<GcBox<T>>,
    _marker: PhantomData<Rc<GcBox<T>>>,
}

impl<T: GcTrace> Gc<T> {
    pub fn new(data: T) -> Self {
        GC_STATE.with(|gc| {
            let mut gc = gc.borrow_mut();
            
            let gcbox = GcBox::new(data);
            gc.insert(gcbox);
            Self::from_raw(gcbox)
        })
    }
}

impl<T> Gc<T> where 
    T: GcTrace + ?Sized + Pointee, 
    GcBox<T>: Pointee<Metadata = T::Metadata> 
{
    pub fn from_box(data: Box<T>) -> Self {
        GC_STATE.with(|gc| {
            let mut gc = gc.borrow_mut();
            
            let gcbox = GcBox::from_box(data);
            gc.insert(gcbox);
            Self::from_raw(gcbox)
        })
    }
}

impl<T> Gc<T> where T: GcTrace + ?Sized {
    
    pub(super) fn from_raw(ptr: NonNull<GcBox<T>>) -> Self {
        Self { ptr, _marker: PhantomData }
    }
    
    #[inline]
    pub(super) fn inner(&self) -> &GcBox<T> {
        // must not deref during sweep. This should only be possible if called inside a Drop impl
        debug_assert!(deref_safe());
        unsafe { self.ptr.as_ref() }
    }
    
    fn inner_mut(&mut self) -> &mut GcBox<T> {
        debug_assert!(deref_safe());
        unsafe { self.ptr.as_mut() }
    }
    
    pub fn mark_trace(mut self) {
        self.inner_mut().mark_trace()
    }
    
    /// Create a weak reference from this GC handle
    pub fn weakref(&self) -> GcWeak<T> {
        let weak_ptr = GcBox::get_or_make_weak(self.ptr);
        GcWeak::new(Gc::from_raw(weak_ptr))
    }
    
    pub fn ptr_eq<U>(self_gc: &Gc<T>, other_gc: &Gc<U>) -> bool where U: GcTrace + ?Sized {
        ptr::eq(
            self_gc.ptr.as_ptr() as *const (),
            other_gc.ptr.as_ptr() as *const (),
        )
    }
    
    /// Casts the inner pointer to a usize. 
    /// This is intended for identifying the Gc, and should not be cast back to a pointer.
    pub fn as_id(self_gc: &Gc<T>) -> usize {
        self_gc.ptr.as_ptr() as *const () as usize
    }

}

impl<T> From<Gc<T>> for Gc<dyn GcTrace> where T: GcTrace {
    fn from(handle: Gc<T>) -> Self {
        Self {
            ptr: handle.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T> AsRef<T> for Gc<T> where T: GcTrace + ?Sized {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<T> Borrow<T> for Gc<T> where T: GcTrace + ?Sized {
    fn borrow(&self) -> &T {
        self.deref()
    }
}

impl<T> Deref for Gc<T> where T: GcTrace + ?Sized {
    type Target = T;
    
    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner().value()
    }
}

impl<T> Clone for Gc<T> where T: GcTrace + ?Sized {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T> Copy for Gc<T> where T: GcTrace + ?Sized { }

impl<T> Hash for Gc<T> where T: GcTrace {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        <NonNull<GcBox<T>> as Hash>::hash(&self.ptr, state)
    }
}

impl<T> fmt::Debug for Gc<T> where T: GcTrace + ?Sized {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("Gc")
            .field(&self.ptr)
            .finish()
    }
}


#[derive(Clone, Copy)]
pub struct GcWeak<T> where T: GcTrace + ?Sized + 'static {
    gc_weak: Gc<GcWeakCell<T>>,
}

impl<T> GcWeak<T> where T: GcTrace + ?Sized {
    fn new(gc_weak: Gc<GcWeakCell<T>>) -> Self {
        Self { gc_weak }
    }
    
    pub fn is_valid(&self) -> bool {
        self.gc_weak.get().is_some()
    }
    
    pub fn try_deref(&self) -> Option<&T> {
        self.gc_weak.get().map(|ptr| {
            // must not deref during sweep. This should only be possible if called inside a Drop impl
            debug_assert!(deref_safe());
            let gcbox = unsafe { ptr.as_ref() };
            gcbox.value()
        })
    }
    
    // This marks the allocation for the weak reference - NOT the referent of the weak reference
    pub fn mark_trace(&self) {
        self.gc_weak.mark_trace()
    }
    
    pub fn ptr_eq<U>(self_weak: &GcWeak<T>, other_weak: &GcWeak<U>) -> bool where U: GcTrace + ?Sized {
        Gc::ptr_eq(&self_weak.gc_weak, &other_weak.gc_weak)
    }
    
    /// This is intended for identifying the GcWeak, and should not be cast back to a pointer.
    pub fn as_id(self_weak: &GcWeak<T>) -> usize {
        Gc::as_id(&self_weak.gc_weak)
    }
}

impl<T> fmt::Debug for GcWeak<T> where T: GcTrace + ?Sized {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("GcWeak")
            .field(&self.gc_weak.ptr)
            .finish()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use core::cell::Cell;
    use crate::runtime::gc::gc_force;
    // use test_log::test;
    
    #[test]
    fn test_weak_ref_dereference() {
        let data = Gc::new(1);
        let weak = data.weakref();
        
        assert!(matches!(weak.try_deref(), Some(1)));
        
        gc_force(&0); //cleanup so miri doesn't complain about leaks
    }
    
    #[test]
    fn test_weak_ref_dereference_mut() {
        let data = Gc::new(Cell::new(2));
        let weak = data.weakref();
        
        let cell = weak.try_deref().unwrap();
        assert!(cell.get() == 2);
        
        cell.set(cell.get() + 1);
        assert!(cell.get() == 3);
        
        gc_force(&0); //cleanup so miri doesn't complain about leaks
    }
    
    #[test]
    fn test_weak_ref_invalidated() {
        let data = Gc::new(3);
        let weak = data.weakref();
        assert!(weak.is_valid());
        
        weak.mark_trace();
        
        gc_force(&0);
        
        assert!(!weak.is_valid());
        
        gc_force(&0); //cleanup so miri doesn't complain about leaks
    }
    
    #[test]
    fn test_weak_ref_reclaimed() {
        let data = Gc::new(4);
        assert!(data.inner().header().weak().is_none());
        
        data.weakref();
        assert!(data.inner().header().weak().is_some());
        
        data.mark_trace();
        
        gc_force(&0);
        
        assert!(data.inner().header().weak().is_none());
        
        gc_force(&0); //cleanup so miri doesn't complain about leaks
    }
}