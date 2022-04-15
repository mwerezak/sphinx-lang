use core::fmt;
use core::ops::Deref;
use core::borrow::Borrow;
use core::ptr::NonNull;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use std::rc::Rc;

use crate::runtime::gc::{GCBox, GC_STATE, deref_safe, GCTrace};


pub struct GC<T> where T: GCTrace + ?Sized + 'static {
    ptr: NonNull<GCBox<T>>,
    _marker: PhantomData<Rc<GCBox<T>>>,
}

impl<T: GCTrace> GC<T> {
    pub fn new(data: T) -> Self {
        GC_STATE.with(|gc| {
            let mut gc = gc.borrow_mut();
            Self::from_raw(gc.allocate(data))
        })
    }
}

impl<T> GC<T> where T: GCTrace + ?Sized {
    pub(super) fn from_raw(ptr: NonNull<GCBox<T>>) -> Self {
        Self { ptr, _marker: PhantomData }
    }
    
    #[inline]
    pub(super) fn inner(&self) -> &GCBox<T> {
        // must not deref during sweep. This should only be possible if called inside a Drop impl
        debug_assert!(deref_safe());
        unsafe { &*self.ptr.as_ptr() }
    }
    
    #[inline]
    fn inner_mut(&self) -> &mut GCBox<T> {
        // must not deref during sweep. This should only be possible if called inside a Drop impl
        debug_assert!(deref_safe());
        unsafe { &mut *self.ptr.as_ptr() }
    }
    
    pub fn mark_trace(&self) {
        self.inner_mut().mark_trace()
    }
    
    pub fn ptr_eq(self_gc: &GC<T>, other_gc: &GC<T>) -> bool {
        self_gc.inner().ptr_eq(other_gc.inner())
    }
    
    /// Casts the inner pointer to a usize. 
    /// This is intended for identifying the GC, and should not be cast back to a pointer.
    pub fn as_id(self_gc: &GC<T>) -> usize {
        self_gc.ptr.as_ptr() as *const () as usize
    }

}

impl<T> From<GC<T>> for GC<dyn GCTrace> where T: GCTrace {
    fn from(handle: GC<T>) -> Self {
        Self {
            ptr: handle.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T> AsRef<T> for GC<T> where T: GCTrace + ?Sized {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<T> Borrow<T> for GC<T> where T: GCTrace + ?Sized {
    fn borrow(&self) -> &T {
        self.deref()
    }
}

impl<T> Deref for GC<T> where T: GCTrace + ?Sized {
    type Target = T;
    
    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner().value()
    }
}

impl<T> Clone for GC<T> where T: GCTrace + ?Sized {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T> Copy for GC<T> where T: GCTrace + ?Sized { }

impl<T> Hash for GC<T> where T: GCTrace {
    fn hash<H>(self: &Self, state: &mut H) where H: Hasher {
        <NonNull<GCBox<T>> as Hash>::hash(&self.ptr, state)
    }
}

impl<T> fmt::Debug for GC<T> where T: GCTrace + ?Sized {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        if fmt.alternate() {
            write!(fmt, "GC({:#?})", self.ptr)
        } else {
            write!(fmt, "GC({:?})", self.ptr)
        }
    }
}
