use core::fmt;
use core::ops::Deref;
use core::borrow::Borrow;
use core::ptr::{NonNull, Pointee};
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use std::rc::Rc;

use crate::runtime::gc::{GcBox, GcBoxHeader, GC_STATE, deref_safe, GcTrace};


pub struct Gc<T> where T: GcTrace + ?Sized + 'static {
    ptr: NonNull<GcBox<T>>,
    _marker: PhantomData<Rc<GcBox<T>>>,
}

impl<T: GcTrace> Gc<T> {
    pub fn new(data: T) -> Self {
        GC_STATE.with(|gc| {
            let mut gc = gc.borrow_mut();
            
            let gcbox = GcBox::new(data);
            let header = GcBoxHeader::from_alloc(gcbox);
            gc.insert(header);
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
            let header = GcBoxHeader::from_alloc(gcbox);
            gc.insert(header);
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
        unsafe { &*self.ptr.as_ptr() }
    }
    
    #[inline]
    fn inner_mut(&self) -> &mut GcBox<T> {
        // must not deref during sweep. This should only be possible if called inside a Drop impl
        debug_assert!(deref_safe());
        unsafe { &mut *self.ptr.as_ptr() }
    }
    
    pub fn mark_trace(&self) {
        self.inner_mut().mark_trace()
    }
    
    pub fn ptr_eq(self_gc: &Gc<T>, other_gc: &Gc<T>) -> bool {
        self_gc.inner().ptr_eq(other_gc.inner())
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
    fn hash<H>(self: &Self, state: &mut H) where H: Hasher {
        <NonNull<GcBox<T>> as Hash>::hash(&self.ptr, state)
    }
}

impl<T> fmt::Debug for Gc<T> where T: GcTrace + ?Sized {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        if fmt.alternate() {
            write!(fmt, "Gc({:#?})", self.ptr)
        } else {
            write!(fmt, "Gc({:?})", self.ptr)
        }
    }
}
