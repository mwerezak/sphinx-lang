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


struct GcBoxHeader {
    next: Option<NonNull<GcBox<dyn GcTrace>>>,
    marked: bool,
}

// need to control memory layout to alloc for unsized data
#[repr(C)]
pub struct GcBox<T> where T: GcTrace + ?Sized + 'static {
    header: GcBoxHeader,
    data: T,
}

// constructor for sized types
impl<T> GcBox<T> where T: GcTrace {
    pub fn new(data: T) -> NonNull<GcBox<T>> {
        let gcbox = Box::new(GcBox {
            header: GcBoxHeader {
                next: None,
                marked: false,
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
        let data_size = mem::size_of_val(&*data);
        let data_ptr = Box::into_raw(data);
        let ptr: *mut GcBox<T> = ptr::from_raw_parts_mut::<GcBox<T>>(
            ptr as *mut (), ptr::metadata(data_ptr)
        );
        
        // initialize the GcBox
        let header = GcBoxHeader {
            next: None,
            marked: false,
        };
        
        unsafe {
            ptr::write(&mut (*ptr).header, header);
            // seems a little sketchy
            ptr::copy_nonoverlapping(data_ptr as *mut u8, ptr::addr_of_mut!((*ptr).data) as *mut u8, data_size);
        }
        
        // manually drop the original box
        unsafe {
            dealloc(data_ptr as *mut u8, data_layout);
        }
        
        // SAFETY: Checked ptr.is_null() earlier.
        unsafe { NonNull::new_unchecked(ptr) }
    }
}

// fn bar<T: ?Sized>(ptr: *mut T) -> *mut Foo<T> 
// where
//     T: Pointee,
//     Foo<T>: Pointee<Metadata = T::Metadata>,
// {
//     ptr::from_raw_parts_mut(
//         ptr as *mut (),
//         ptr::metadata(ptr),
//     )
// }

/*
expected associated type `<GcBox<T> as Pointee>::Metadata`
   found associated type `<T as Pointee>::Metadata`
*/

impl<T> GcBox<T> where T: GcTrace + ?Sized {
    #[inline]
    pub fn value(&self) -> &T { &self.data }
    
    #[inline]
    pub fn next(&self) -> Option<NonNull<GcBox<dyn GcTrace>>> {
        self.header.next
    }
    
    #[inline]
    pub fn set_next(&mut self, next: Option<NonNull<GcBox<dyn GcTrace>>>) {
        self.header.next = next
    }
    
    #[inline]
    pub fn size(&self) -> usize {
        mem::size_of_val(self) + self.value().size_hint()
    }
    
    #[inline]
    pub fn ptr_eq(&self, other: &GcBox<T>) -> bool {
        ptr::eq(&self.header, &other.header)
    }
    
    #[inline]
    pub fn is_marked(&self) -> bool {
        self.header.marked
    }
    
    #[inline]
    pub fn mark_trace(&mut self) {
        if !self.header.marked {
            self.header.marked = true;
            self.data.trace();
        }
    }
    
    #[inline]
    pub fn clear_mark(&mut self) {
        self.header.marked = false
    }
}
