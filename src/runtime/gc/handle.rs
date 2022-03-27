use std::rc::Rc;
use std::ops::Deref;
use std::ptr::NonNull;
use std::marker::PhantomData;

use crate::runtime::gc::{GCBox, GC_STATE, deref_safe};



#[derive(Debug)]
pub struct GCHandle<T> where T: 'static {
    ptr: NonNull<GCBox<T>>,
    _marker: PhantomData<Rc<GCBox<T>>>,
}

impl<T> GCHandle<T> {
    fn from_raw(ptr: NonNull<GCBox<T>>) -> Self {
        Self { ptr, _marker: PhantomData }
    }
    
    pub fn allocate(data: T) -> Self {
        GC_STATE.with(|gc| {
            let mut gc = gc.borrow_mut();
            Self::from_raw(gc.allocate(data))
        })
    }
    
    #[inline]
    fn inner(&self) -> &GCBox<T> {
        // must not deref GCHandles during sweep. This should only be possible in a Drop impl
        debug_assert!(deref_safe());
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl<T> Clone for GCHandle<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T> Deref for GCHandle<T> {
    type Target = T;
    
    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner().value()
    }
}