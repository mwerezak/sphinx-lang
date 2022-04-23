use log;
use core::fmt;
use core::mem;
use core::cell::{Cell, RefCell};
use core::alloc::Layout;
use core::ptr::{self, NonNull, Pointee};
use std::alloc::{self, alloc, dealloc};


/// Unsafe because if the `trace()` implementation fails to call `Gc::mark_trace()` 
/// and `GcWeak::mark_trace()` on all of the `Gc` and `GcWeak` pointers that it can reach,
/// the GC will free memory that is still in use.
/// SAFETY: If the receiver also impls `Drop`, the `drop()` impl must not deref any `Gc` or `GcWeak` pointers
/// It is recommended that `GcTrace` should not implemented for any types that also impl Drop
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


/// allows GcBoxHeader to invalidate the weak cell without knowing the type
pub(super) trait WeakCell: GcTrace {
    fn invalidate(&self);
}


/// used by GcState to store GcBox<T> of different types
#[derive(Debug, Clone, Copy)]
pub(super) struct GcBoxPtr {
    ptr: NonNull<GcBoxHeader>,
}

impl GcBoxPtr {
    #[inline]
    pub(super) fn new<T>(gcbox: NonNull<GcBox<T>>) -> Self where T: GcTrace + ?Sized {
        Self { ptr: gcbox.cast() }
    }
    
    #[inline]
    pub(super) unsafe fn header(&self) -> &GcBoxHeader {
        self.ptr.as_ref()
    }
    
    #[inline]
    pub(super) unsafe fn header_mut(&mut self) -> &mut GcBoxHeader {
        self.ptr.as_mut()
    }
    
    #[inline]
    pub(super) fn as_ptr<T>(&self) -> *mut T {
        self.ptr.as_ptr() as *mut T
    }
    
    #[inline]
    pub(super) unsafe fn free(self) -> Option<GcBoxPtr> {
        free_gcbox(self)
    }
}

pub(super) struct GcBoxHeader {
    next: Option<GcBoxPtr>,
    marked: bool,
    size: usize,
    layout: Layout,
    weak: Option<NonNull<GcBox<dyn WeakCell>>>,
    destructor: Option<Box<dyn Fn(GcBoxPtr)>>,
}

impl GcBoxHeader {
    #[inline]
    pub(super) fn next(&self) -> Option<GcBoxPtr> {
        self.next
    }
    
    #[inline]
    pub(super) fn set_next(&mut self, next: Option<GcBoxPtr>) {
        self.next = next
    }
    
    #[inline]
    pub(super) fn size(&self) -> usize {
        self.size
    }
    
    #[inline]
    pub(super) fn is_marked(&self) -> bool {
        self.marked
    }
    
    #[inline]
    pub(super) fn set_marked(&mut self, marked: bool) {
        self.marked = marked
    }
    
    #[inline]
    pub(super) fn weak(&self) -> Option<NonNull<GcBox<dyn WeakCell>>> {
        self.weak
    }
    
    #[inline]
    pub(super) fn set_weak(&mut self, weak: Option<NonNull<GcBox<dyn WeakCell>>>) {
        self.weak = weak
    }
    
    #[inline]
    fn take_destructor(&mut self) -> Option<Box<dyn Fn(GcBoxPtr)>> {
        self.destructor.take()
    }
}

impl Drop for GcBoxHeader {
    fn drop(&mut self) {
        if let Some(weak_ptr) = self.weak.take() {
            unsafe { weak_ptr.as_ref() }
                .value().invalidate();
        }
    }
}

impl fmt::Debug for GcBoxHeader {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("GcBoxHeader")
            .field("next", &self.next)
            .field("size", &self.size)
            .field("layout", &self.layout)
            .field("destructor", &self.destructor.as_ref()
                .map(|destfn| ptr::addr_of!(*destfn)))
            .finish()
    }
}

// need to control memory layout to alloc for unsized data
#[repr(C)]
#[derive(Debug)]
pub struct GcBox<T> where T: GcTrace + ?Sized + 'static {
    header: GcBoxHeader,
    data: T,
}

// constructor for sized types
impl<T> GcBox<T> where T: GcTrace {
    pub(super) fn new(data: T) -> NonNull<GcBox<T>> {
        if mem::size_of::<T>() == 0 {
            panic!("gc alloc zero-sized type")
        }
        
        let layout = Layout::new::<GcBox<T>>();
        let size = layout.size() + data.size_hint();
        let destructor = |ptr: GcBoxPtr| unsafe {
            log::debug!("{:#X} run sized destructor", ptr.as_ptr::<()>() as usize);
            ptr::drop_in_place::<GcBox<T>>(ptr.as_ptr())
        };
        
        let gcbox = Box::new(GcBox {
            header: GcBoxHeader {
                next: None,
                marked: false,
                size, layout,
                weak: None,
                destructor: Some(Box::new(destructor)),
            },
            data,
        });
        
        unsafe { NonNull::new_unchecked(Box::into_raw(gcbox)) }
    }
}

impl<T> GcBox<T> where 
    T: GcTrace + ?Sized + Pointee + 'static, 
    GcBox<T>: Pointee<Metadata = T::Metadata> 
{
    pub(super) fn from_box(data: Box<T>) -> NonNull<GcBox<T>> {
        let size_hint = data.size_hint();
        let data_size = mem::size_of_val(&*data);
        
        if data_size == 0 {
            panic!("gc alloc zero-sized value")
        }
        
        // copy layout of data
        let data_layout = Layout::for_value::<T>(&*data);
        
        // build GcBox layout
        let (layout, _) = Layout::new::<GcBoxHeader>()
            .extend(data_layout).unwrap();
        let layout = layout.pad_to_align();
        
        // allocate
        let ptr = unsafe { alloc(layout) };
        if ptr.is_null() {
            alloc::handle_alloc_error(layout);
        }
        
        // copy metadata from source pointer
        let data_ptr = Box::into_raw(data);
        let ptr_meta = ptr::metadata(data_ptr);
        
        let ptr: *mut GcBox<T> = ptr::from_raw_parts_mut::<GcBox<T>>(
            ptr as *mut (), ptr_meta
        );
        
        // initialize the GcBox
        let destructor = move |ptr: GcBoxPtr| unsafe {
            log::debug!("{:#X} run unsized destructor", ptr.as_ptr::<()>() as usize);
            let ptr = ptr::from_raw_parts_mut::<GcBox<T>>(ptr.as_ptr(), ptr_meta);
            ptr::drop_in_place(ptr);
        };
        
        let header = GcBoxHeader {
            next: None,
            marked: false,
            size: layout.size() + size_hint,
            layout,
            weak: None,
            destructor: Some(Box::new(destructor)),
        };
        
        unsafe {
            ptr::write(&mut (*ptr).header, header);
            ptr::copy_nonoverlapping(
                data_ptr as *mut u8,
                ptr::addr_of_mut!((*ptr).data) as *mut u8, 
                data_size
            );
        }
        
        // manually free the original box
        unsafe { dealloc(data_ptr as *mut u8, data_layout); }
        
        // SAFETY: Checked ptr.is_null() earlier.
        unsafe { NonNull::new_unchecked(ptr) }
    }
}

pub(super) unsafe fn free_gcbox(mut ptr: GcBoxPtr) -> Option<GcBoxPtr> {
    let next = ptr.header().next;
    let layout = ptr.header().layout;
    
    // just in case, we take the destructor out of the gcbox
    // so it doesn't drop itself while being executed
    // this also prevents a GcBox's destructor from being called twice
    let cleanup_fn = ptr.header_mut().take_destructor();
    if let Some(cleanup_fn) = cleanup_fn {
        cleanup_fn(ptr)
    }
    
    // assert that any weak ref has been cleaned up
    debug_assert!(ptr.header().weak().is_none());
    
    dealloc(ptr.as_ptr() as *mut u8, layout);
    
    next
}

impl<T> GcBox<T> where T: GcTrace + ?Sized {
    #[inline]
    pub(super) fn header(&self) -> &GcBoxHeader { &self.header }

    #[inline]
    pub(super) fn header_mut(&mut self) -> &mut GcBoxHeader { &mut self.header }

    #[inline]
    pub(super) fn value(&self) -> &T { &self.data }
    
    #[inline]
    pub(super) fn mark_trace(&mut self) {
        if !self.header.is_marked() {
            self.header.set_marked(true);
            self.data.trace();
        }
    }
}


#[cfg(test)]
mod tests {
    use core::cell::Cell;
    use std::sync::Mutex;
    use once_cell::sync::Lazy;
    use super::*;
    
    unsafe impl GcTrace for i32 {
        fn trace(&self) { }
    }
    
    #[test]
    fn test_gcbox_alloc_dealloc() {
        let data = 63;
        let gcbox = GcBox::new(data);
        println!("gcbox sized: {:#?}", unsafe { gcbox.as_ref() });
        unsafe { free_gcbox(GcBoxPtr::new(gcbox)); }
    }
    
    #[test]
    fn test_gcbox_alloc_dealloc_unsized() {
        let unsized_data = vec![1,2,3,4,5].into_boxed_slice();
        let gcbox = GcBox::from_box(unsized_data);
        println!("gcbox unsized: {:#?}", unsafe { gcbox.as_ref() });
        unsafe { free_gcbox(GcBoxPtr::new(gcbox)); }
    }
    
    #[test]
    fn test_gcbox_destructor() {
        static DROPPED: Lazy<Mutex<Cell<bool>>> = Lazy::new(|| Mutex::new(Cell::new(false)));
        
        #[derive(Debug)]
        struct DropTest(&'static Mutex<Cell<bool>>);
        
        unsafe impl GcTrace for DropTest {
            fn trace(&self) { }
        }
        
        impl Drop for DropTest {
            fn drop(&mut self) {
                println!("dropping {:?}", self);
                (*self.0).lock().unwrap().set(true);
            }
        }
        
        let test = DropTest(&DROPPED);
        let gcbox = GcBox::new(test);
        println!("gcbox sized: {:#?}", unsafe { gcbox.as_ref() });
        unsafe { free_gcbox(GcBoxPtr::new(gcbox)); }
        
        assert!(DROPPED.lock().unwrap().get())
    }
}