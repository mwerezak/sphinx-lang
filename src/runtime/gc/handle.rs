use std::fmt;
use std::rc::Rc;
use std::ops::Deref;
use std::ptr::{self, NonNull};
use std::marker::PhantomData;

use crate::runtime::gc::{GCBox, GCArray, GC_STATE, deref_safe, GCTrace};


pub struct GC<T> where T: GCTrace + ?Sized + 'static {
    ptr: NonNull<GCBox<T>>,
    _marker: PhantomData<Rc<GCBox<T>>>,
}

impl<T> GC<T> where T: GCTrace + ?Sized {
    pub(super) fn from_raw(ptr: NonNull<GCBox<T>>) -> Self {
        Self { ptr, _marker: PhantomData }
    }
    
    #[inline]
    pub(super) fn inner(&self) -> &GCBox<T> {
        // must not deref during sweep. This should only be possible in a Drop impl
        debug_assert!(deref_safe());
        unsafe { &*self.ptr.as_ptr() }
    }
    
    pub fn ptr_eq(self_gc: &GC<T>, other_gc: &GC<T>) -> bool {
        self_gc.inner().ptr_eq(other_gc.inner())
    }
}

impl<T: GCTrace> GC<T> {
    pub fn allocate(data: T) -> Self {
        GC_STATE.with(|gc| {
            let mut gc = gc.borrow_mut();
            Self::from_raw(gc.allocate(data))
        })
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

// impl<T> From<Box<T>> for GC<T> where T: ?Sized {
//     fn from(data: Box<T>) -> Self {
//         unimplemented!()
//     }
// }


impl<T> Clone for GC<T> where T: GCTrace + ?Sized {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T> Copy for GC<T> where T: GCTrace + ?Sized { }


impl<T> Deref for GC<T> where T: GCTrace + ?Sized {
    type Target = T;
    
    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner().value()
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
