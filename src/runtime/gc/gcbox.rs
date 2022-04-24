use core::fmt;
use core::mem;
use core::alloc::Layout;
use core::ptr::{self, NonNull, Pointee};
use std::alloc::{self, alloc, dealloc};
use log;

use crate::runtime::gc::trace::GcTrace;
use crate::runtime::gc::ptrmeta::PtrMetadata;


/// Non-generic pointer to a [GcBox<T>].
/// Because GC data may be unsized, we can't rely on trait objects to store mixed `GcBox<T>` structs in a collection
/// Therefore, `GcState` tracks all of the allocations using `repr(C)` type punning, which allows us to freely cast a
/// `*mut GcBox<T>` into a `*mut GcBoxHeader` (and back). There are two consequences of this:
///
/// 1) `GcBoxHeader` must not be generic (or else we're just back to our original problem).
/// 2) When dealing with mixed `GcBox<T>`s, the GC can only access the header.
///
/// Therefore the header must provide all the functionality needed by GcState without using generics.
/// In practice this means:
///
/// - Reading and mutating the "marked" flag.
/// - Freeing the allocation (including running destructors).
/// - Getting the "next" allocation (for use as an intrusive list).
/// - Getting the DST metadata (to support the [`Gc<T>`] thin pointer representation).
///
#[derive(Debug, Clone, Copy, Hash)]
pub(super) struct GcBoxPtr {
    ptr: NonNull<GcBoxHeader>,
}

impl<T> From<NonNull<GcBox<T>>> for GcBoxPtr where T: GcTrace + ?Sized {
    fn from(gcbox: NonNull<GcBox<T>>) -> Self {
        Self { ptr: gcbox.cast() }
    }
}

impl GcBoxPtr {
    #[inline]
    pub(super) unsafe fn header(&self) -> &GcBoxHeader {
        self.ptr.as_ref()
    }
    
    #[inline]
    pub(super) unsafe fn header_mut(&mut self) -> &mut GcBoxHeader {
        self.ptr.as_mut()
    }
    
    #[inline]
    pub(super) fn as_ptr(&self) -> *mut GcBoxHeader {
        self.ptr.as_ptr()
    }
    
    pub(super) fn to_gcbox_ptr<T>(self) -> NonNull<GcBox<T>> where 
        T: GcTrace + ?Sized,
        PtrMetadata: TryInto<<GcBox<T> as Pointee>::Metadata>
    {
        // retrieve metadata and construct pointer
        let metadata = unsafe { self.header().metadata() };
        let ptr_meta = metadata.try_into()
            .ok().expect("invalid pointer metadata");
        
        let ptr = ptr::from_raw_parts_mut::<GcBox<T>>(
            self.as_ptr() as *mut (), ptr_meta
        );
        unsafe { NonNull::new_unchecked(ptr) }
    }
}


/// Allows [GcBoxHeader] to invalidate a [GcWeakCell<T>] without knowing the type of `T`
pub(super) trait WeakCell: GcTrace {
    fn invalidate(&self);
}

pub(super) struct GcBoxHeader {
    next: Option<GcBoxPtr>,
    marked: bool,
    size: usize,
    layout: Layout,
    metadata: PtrMetadata,
    weak: Option<NonNull<GcBox<dyn WeakCell>>>,
    destructor: Option<Box<dyn Fn(GcBoxPtr)>>,
}

impl GcBoxHeader {
    fn new(size: usize, layout: Layout, metadata: PtrMetadata, destructor: Box<dyn Fn(GcBoxPtr)>) -> Self {
        Self {
            next: None,
            marked: false,
            size, layout,
            metadata,
            weak: None,
            destructor: Some(destructor),
        }
    }
    
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
    pub(super) fn metadata(&self) -> PtrMetadata {
        self.metadata
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

// repr(C) is used to control memory layout to alloc for unsized data, and for GcBoxPtr type punning
#[repr(C)]
#[derive(Debug)]
pub struct GcBox<T> where T: GcTrace + ?Sized + 'static {
    header: GcBoxHeader,
    data: T,
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

// Allocation and Deallocation

// constructor for sized types
impl<T> GcBox<T> where 
    T: GcTrace + Pointee,
    T::Metadata: Into<PtrMetadata>,
{
    pub(super) fn new(data: T) -> NonNull<GcBox<T>> {
        if mem::size_of::<T>() == 0 {
            panic!("gc alloc zero-sized type")
        }
        
        let layout = Layout::new::<GcBox<T>>();
        let size = layout.size() + data.size_hint();
        let ptr_meta = ptr::metadata(&data); // should just be (), but better to be safe
        let destructor = |ptr: GcBoxPtr| unsafe {
            log::debug!("{:#X} run sized destructor", ptr.as_ptr() as usize);
            ptr::drop_in_place::<GcBox<T>>(ptr.as_ptr() as *mut GcBox<T>)
        };
        
        let header = GcBoxHeader::new(
            size,
            layout,
            ptr_meta.into(),
            Box::new(destructor)
        );
        
        let gcbox = Box::new(GcBox { header, data });
        
        unsafe { NonNull::new_unchecked(Box::into_raw(gcbox)) }
    }
}

impl<T> GcBox<T> where 
    T: GcTrace + ?Sized + Pointee + 'static,
    T::Metadata: Into<PtrMetadata>,
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
            log::debug!("{:#X} run unsized destructor", ptr.as_ptr() as usize);
            let ptr = ptr::from_raw_parts_mut::<GcBox<T>>(
                ptr.as_ptr() as *mut (), ptr_meta
            );
            ptr::drop_in_place(ptr);
        };
        
        let header = GcBoxHeader::new(
            layout.size() + size_hint, 
            layout, 
            ptr_meta.into(),
            Box::new(destructor)
        );
        
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

impl<T> GcBox<T> where T: GcTrace + ?Sized {
    pub(super) unsafe fn free(self_ptr: NonNull<Self>) -> Option<GcBoxPtr> {
        GcBoxPtr::from(self_ptr).free()
    }
}

impl GcBoxPtr {
    pub(super) unsafe fn free(mut self) -> Option<GcBoxPtr> {
        let next = self.header().next;
        let layout = self.header().layout;
        
        // just in case, we take the destructor out of the gcbox
        // so it doesn't drop itself while being executed
        // this also prevents a GcBox's destructor from being called twice
        let cleanup_fn = self.header_mut().take_destructor();
        if let Some(cleanup_fn) = cleanup_fn {
            cleanup_fn(self)
        }
        
        // assert that any weak ref has been cleaned up
        debug_assert!(self.header().weak().is_none());
        
        dealloc(self.as_ptr() as *mut u8, layout);
        
        next
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
        unsafe { GcBox::free(gcbox); }
    }
    
    #[test]
    fn test_gcbox_alloc_dealloc_unsized() {
        let unsized_data = vec![1,2,3,4,5].into_boxed_slice();
        let gcbox = GcBox::from_box(unsized_data);
        println!("gcbox unsized: {:#?}", unsafe { gcbox.as_ref() });
        unsafe { GcBox::free(gcbox); }
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
        unsafe { GcBox::free(gcbox); }
        
        assert!(DROPPED.lock().unwrap().get())
    }
}