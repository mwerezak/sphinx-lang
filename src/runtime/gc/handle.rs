use std::fmt;
use std::rc::Rc;
use std::ops::Deref;
use std::ptr::NonNull;
use std::marker::PhantomData;

use crate::runtime::gc::{GCBox, GCArray, GC_STATE, deref_safe, SizeOf};



pub struct GCHandle<T> where T: ?Sized + 'static {
    ptr: NonNull<GCBox<T>>,
    _marker: PhantomData<Rc<GCBox<T>>>,
}

impl<T> GCHandle<T> where T: ?Sized {
    pub(super) fn from_raw(ptr: NonNull<GCBox<T>>) -> Self {
        Self { ptr, _marker: PhantomData }
    }
    
    #[inline]
    pub(super) fn inner(&self) -> &GCBox<T> {
        // must not deref during sweep. This should only be possible in a Drop impl
        debug_assert!(deref_safe());
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl<T: SizeOf> GCHandle<T> {
    pub fn allocate(data: T) -> Self {
        GC_STATE.with(|gc| {
            let mut gc = gc.borrow_mut();
            Self::from_raw(gc.allocate(data))
        })
    }
}

// impl<T> From<Box<T>> for GCHandle<T> where T: ?Sized {
//     fn from(data: Box<T>) -> Self {
//         unimplemented!()
//     }
// }


impl<T> Clone for GCHandle<T> where T: ?Sized {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T> Deref for GCHandle<T> where T: ?Sized {
    type Target = T;
    
    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner().value()
    }
}

impl<T> fmt::Debug for GCHandle<T> where T: ?Sized {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        if fmt.alternate() {
            write!(fmt, "GCHandle({:#?})", self.ptr)
        } else {
            write!(fmt, "GCHandle({:?})", self.ptr)
        }
    }
}
