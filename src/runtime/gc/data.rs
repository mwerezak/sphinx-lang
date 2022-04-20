use core::fmt;
use core::mem::{self, MaybeUninit};
use core::alloc::Layout;
use core::ptr::{self, NonNull, Pointee};
use core::cell::{Cell, RefCell};
use std::alloc::{self, alloc, dealloc};


/// Unsafe because if the GcTrace::trace() implementation fails to mark any Gc handles that it can reach, 
/// the Gc will not be able to mark them and will free memory that is still in use.
/// SAFETY: Must not impl Drop
pub unsafe trait GcTrace {
    
    /// SAFETY: Must call `Gc::mark_trace()` on every reachable Gc handle
    fn trace(&self);
    
    /// If the GcTrace owns any allocations, this should return the extra allocated size.
    /// If the allocation can change size, like a Vec<T>, then don't include it in the 
    /// size hint, or return a const estimate of the average size.
    #[inline]
    fn size_hint(&self) -> usize { 0 }
}

// arrays
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

#[derive(Debug)]
pub struct GcBoxHeader {
    next: Option<NonNull<GcBoxHeader>>,
    marked: bool,
    size: usize,
    layout: Layout,
}

impl GcBoxHeader {
    #[inline]
    pub fn from_alloc<T>(gcbox: NonNull<GcBox<T>>) -> NonNull<Self> where T: GcTrace + ?Sized {
        unsafe { NonNull::new_unchecked(gcbox.as_ptr() as *mut GcBoxHeader) }
    }
    
    #[inline]
    pub fn next(&self) -> Option<NonNull<Self>> {
        self.next
    }
    
    #[inline]
    pub fn set_next(&mut self, next: Option<NonNull<Self>>) {
        self.next = next
    }
    
    #[inline]
    pub fn size(&self) -> usize {
        self.size
    }
    
    #[inline]
    pub fn is_marked(&self) -> bool {
        self.marked
    }
    
    #[inline]
    pub fn set_marked(&mut self, marked: bool) {
        self.marked = marked
    }
    
    /// Frees the entire gcbox
    #[inline]
    pub unsafe fn free(gcbox: NonNull<Self>) -> Option<NonNull<Self>> {
        let next = gcbox.as_ref().next;
        let layout = gcbox.as_ref().layout;
        
        dealloc(gcbox.as_ptr() as *mut u8, layout);
        
        next
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
    pub fn new(data: T) -> NonNull<GcBox<T>> {
        if mem::size_of::<T>() == 0 {
            panic!("gc alloc zero-sized type")
        }
        
        let layout = Layout::new::<GcBox<T>>();
        let size = layout.size() + data.size_hint();
        
        let gcbox = Box::new(GcBox {
            header: GcBoxHeader {
                next: None,
                marked: false,
                size, layout,
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
    pub fn from_box(data: Box<T>) -> NonNull<GcBox<T>> {
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
        let header = GcBoxHeader {
            next: None,
            marked: false,
            size: layout.size() + size_hint,
            layout,
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


impl<T> GcBox<T> where T: GcTrace + ?Sized {
    #[inline]
    pub fn header(&self) -> &GcBoxHeader { &self.header }

    #[inline]
    pub fn value(&self) -> &T { &self.data }
    
    #[inline]
    pub fn mark_trace(&mut self) {
        if !self.header.is_marked() {
            self.header.set_marked(true);
            self.data.trace();
        }
    }
    
    unsafe fn free(gcbox: NonNull<GcBox<T>>) {
        let layout = gcbox.as_ref().header.layout;
        dealloc(gcbox.as_ptr() as *mut u8, layout);
    }
    
}


#[cfg(test)]
mod tests {
    use super::*;
    
    unsafe impl GcTrace for i32 {
        fn trace(&self) {}
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
    fn test_gcbox_alloc_dealloc_from_header() {
        let data = 63;
        let gcbox = GcBox::new(data);
        println!("gcbox sized: {:#?}", unsafe { gcbox.as_ref() });
        unsafe {
            let header = GcBoxHeader::from_alloc(gcbox);
            GcBoxHeader::free(header); 
        }
    }
    
    #[test]
    fn test_gcbox_alloc_dealloc_from_header_unsized() {
        let unsized_data = vec![1,2,3,4,5].into_boxed_slice();
        let gcbox = GcBox::from_box(unsized_data);
        println!("gcbox unsized: {:#?}", unsafe { gcbox.as_ref() });
        unsafe {
            let header = GcBoxHeader::from_alloc(gcbox);
            GcBoxHeader::free(header);
        }
    }
}