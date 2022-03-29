///! Compatibility types to allow GCBox to handle [T]
///! Won't be needed once CoerceUnsized hits stable


use std::fmt;
use std::mem;
use std::ops::Deref;
use std::ptr::NonNull;
use crate::runtime::gc::{GCBox, GC, GC_STATE, deref_safe, GCTrace};


pub(crate) struct Array<T> {
    ptr: NonNull<[T]>,
}

impl<T> From<Box<[T]>> for Array<T> {
    fn from(data: Box<[T]>) -> Self {
        // SAFETY: From the docs for Box::into_raw(), "The pointer will be properly aligned and non-null."
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(data)) };
        Self { ptr }
    }
}

impl<T> From<Array<T>> for Box<[T]> {
    fn from(arr: Array<T>) -> Self {
        // SAFETY: This is safe because ptr is obtained only from Box::into_raw()
        unsafe { Self::from_raw(arr.ptr.as_ptr()) }
    }
}

impl<T> Drop for Array<T> {
    fn drop(&mut self) {
        // SAFETY: This is safe because ptr is obtained only from Box::into_raw()
        unsafe { Box::from_raw(self.ptr.as_ptr()); }
    }
}

impl<T> GCTrace for Array<T> where T: GCTrace {
    fn extra_size(&self) -> usize {
        unsafe { mem::size_of_val(&*self.ptr.as_ptr()) }
    }
}


#[derive(Clone, Copy)]
pub struct GCArray<T> where T: GCTrace + 'static {
    handle: GC<Array<T>>,
    data_ptr: NonNull<[T]>,
}

impl<T> GCArray<T> where T: GCTrace {
    #[inline]
    pub(super) fn inner(&self) -> &GCBox<Array<T>> {
        self.handle.inner()
    }
    
    pub fn from_boxed_slice(data: Box<[T]>) -> Self {
        GC_STATE.with(|gc| {
            let mut gc = gc.borrow_mut();
            
            let array = Array::from(data);
            
            Self {
                data_ptr: array.ptr,
                handle: GC::from_raw(gc.allocate(array)),
            }
        })
    }
}

impl<T> Deref for GCArray<T> where T: GCTrace {
    type Target = [T];
    
    #[inline]
    fn deref(&self) -> &Self::Target {
        // must not deref during sweep. This should only be possible in a Drop impl
        debug_assert!(deref_safe());
        unsafe { &*self.data_ptr.as_ptr() }
    }
}

impl<T> fmt::Debug for GCArray<T> where T: GCTrace {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        if fmt.alternate() {
            write!(fmt, "GCArray({:#?})", self.data_ptr)
        } else {
            write!(fmt, "GCArray({:?})", self.data_ptr)
        }
    }
}