///! The "public API" for the gc is the `Gc<T>` struct, which is a smart pointer to GCed data.
///! Data can be "inserted" into the GC using `Gc::new()` for `Sized` types or `Gc::from_box()` for `?Sized` types.
///! Data is accessed using `Deref`. 
///! Mutable access is not supported, so mutable data that needs to be GCed must use interior mutability.
///! As well, all GCed data must `impl GcTrace`.
///! Weak references to GCed data can be obtained using `Gc::weakref()`.
///! 
///! `Gc<T>` supports a "thin pointer" representation and should not be wider than a single `usize`.

use core::fmt;
use core::ops::Deref;
use core::borrow::Borrow;
use core::ptr::{self, NonNull, Pointee};
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use std::rc::Rc;

use crate::runtime::gc::{GC_STATE, deref_safe};
use crate::runtime::gc::trace::GcTrace;
use crate::runtime::gc::gcbox::{GcBox, GcBoxPtr};
use crate::runtime::gc::ptrmeta::PtrMetadata;
use crate::runtime::gc::weak::GcWeakCell;


///! Smart pointer to GCed data. See the module-level documentation for more details.
pub struct Gc<T> where T: GcTrace + ?Sized + 'static {
    ptr: GcBoxPtr,
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
    T::Metadata: Into<PtrMetadata>,
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
        Self { 
            ptr: ptr.into(),
            _marker: PhantomData,
        }
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

impl<T> Gc<T> where 
    T: GcTrace + ?Sized, 
    PtrMetadata: TryInto<<GcBox<T> as Pointee>::Metadata>
{
    #[inline]
    fn inner(&self) -> &GcBox<T> {
        // must not deref during sweep. This should only be possible if called inside a Drop impl
        debug_assert!(deref_safe());
        unsafe { self.ptr.to_gcbox_ptr().as_ref() }
    }
    
    #[inline]
    fn inner_mut(&mut self) -> &mut GcBox<T> {
        debug_assert!(deref_safe());
        unsafe { self.ptr.to_gcbox_ptr().as_mut() }
    }
    
    /// Create a weak reference from this GC handle
    pub fn weakref(&self) -> GcWeak<T> {
        let weak_ptr = GcBox::get_or_make_weak(self.ptr.to_gcbox_ptr());
        GcWeak::new(Gc::from_raw(weak_ptr))
    }
    
    pub fn mark_trace(mut self) {
        self.inner_mut().mark_trace()
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

impl<T> AsRef<T> for Gc<T> where 
    T: GcTrace + ?Sized,
    PtrMetadata: TryInto<<GcBox<T> as Pointee>::Metadata>
{
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<T> Borrow<T> for Gc<T> where 
    T: GcTrace + ?Sized,
    PtrMetadata: TryInto<<GcBox<T> as Pointee>::Metadata>
{
    fn borrow(&self) -> &T {
        self.deref()
    }
}

impl<T> Deref for Gc<T> where 
    T: GcTrace + ?Sized,
    PtrMetadata: TryInto<<GcBox<T> as Pointee>::Metadata>
{
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

impl<T> Hash for Gc<T> where T: GcTrace + ?Sized {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.ptr.hash(state)
    }
}

impl<T> fmt::Debug for Gc<T> where T: GcTrace + ?Sized {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("Gc")
            .field(&self.ptr)
            .finish()
    }
}


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

impl<T> Clone for GcWeak<T> where T: GcTrace + ?Sized {
    fn clone(&self) -> Self {
        Self {
            gc_weak: self.gc_weak.clone()
        }
    }
}

impl<T> Copy for GcWeak<T> where T: GcTrace + ?Sized { }

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
    use static_assertions::assert_eq_size;
    
    assert_eq_size!(Gc<i32>, usize);
    assert_eq_size!(Gc<[i32]>, usize);
    
    // comment this out when using miri
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