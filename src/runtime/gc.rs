/// Partial clone of rust-gc's mark and sweep implementation

use std::mem;
use std::ptr::{self, NonNull};
use std::cell::{Cell, RefCell};

mod handle;
pub use handle::GC;


/// TODO store GCBoxes in linear chunks instead of individual linked nodes
struct GCBox<T> where T: GCTrace + ?Sized + 'static {
    next: Option<NonNull<GCBox<dyn GCTrace>>>,
    marked: bool,
    data: T,
}

impl<T> GCBox<T> where T: GCTrace + ?Sized {
    fn value(&self) -> &T { &self.data }
    
    #[inline]
    fn size(&self) -> usize {
        mem::size_of_val(self) + self.value().size_hint()
    }
    
    fn ptr_eq(&self, other: &GCBox<T>) -> bool {
        // in case T is a trait object, work around for <https://github.com/rust-lang/rust/issues/46139>
        ptr::eq(
            ptr::addr_of!(self.marked),
            ptr::addr_of!(other.marked),
        )
    }
    
    fn mark_trace(&mut self) {
        if !self.marked {
            self.marked = true;
            self.data.trace();
        }
    }
}


/// Unsafe because if the GCTrace::trace() implementation fails to mark any GC handles that it can reach, 
/// the GC will not be able to mark them and will free memory that is still in use.
/// SAFETY: Must not impl Drop
pub unsafe trait GCTrace {
    
    /// SAFETY: Must call `GC::mark_trace()` on every reachable GC handle
    fn trace(&self);
    
    /// If the GCTrace owns any allocations, this should return the extra allocated size.
    /// If the allocation can change size, like a Vec<T>, then don't include it in the 
    /// size hint, or return a const estimate of the average size.
    #[inline]
    fn size_hint(&self) -> usize { 0 }
}


thread_local! {
    static GC_STATE: RefCell<GCState> = RefCell::new(GCState::default());
}

struct GCState {
    stats: GCStats,
    config: GCConfig,
    boxes_start: Option<NonNull<GCBox<dyn GCTrace>>>,
}

#[derive(Debug)]
struct GCStats {
    allocated: usize,
    cycle_count: usize,
}

struct GCConfig {
    threshold: u16,
    pause_factor: u16,  // percent memory use relative to last cycle before starting a new cycle
}

impl Default for GCConfig {
    fn default() -> Self {
        Self {
            threshold: 512, // Small because of stop-the-world. If we go incremental increase this to 8 kiB
            pause_factor: 200,
        }
    }
}

impl Default for GCState {
    fn default() -> Self {
        GCState::new(GCConfig::default())
    }
}

impl GCState {
    fn new(config: GCConfig) -> Self {
        Self {
            config,
            stats: GCStats {
                allocated: 0,
                cycle_count: 0,
            },
            boxes_start: None,
        }
    }
    
    fn should_collect(&self) -> bool {
        false // TODO
    }
    
    fn allocate<T: GCTrace>(&mut self, data: T) -> NonNull<GCBox<T>> {
        if self.should_collect() {
            unimplemented!()
        }
        
        let gcbox = Box::new(GCBox {
            next: self.boxes_start.take(),
            marked: false,
            data,
        });

        let size = gcbox.size();
        let ptr = unsafe {
            NonNull::new_unchecked(Box::into_raw(gcbox))
        };
        
        self.boxes_start = Some(ptr);
        self.stats.allocated += size;
        
        ptr
    }
    
    /// frees the GCBox, yielding it's next pointer
    fn free(&mut self, gcbox: NonNull<GCBox<dyn GCTrace>>) -> Option<NonNull<GCBox<dyn GCTrace>>> {
        // SAFETY: This is safe as long as we only ever free() GCBoxes that were created by allocate()
        let gcbox = unsafe { Box::from_raw(gcbox.as_ptr()) };
        
        let size = gcbox.size();
        self.stats.allocated -= size;
        
        gcbox.next
        
        // gcbox should get dropped here
    }
    
    fn collect_garbage(&mut self, roots: impl Iterator<Item=GC<dyn GCTrace>>) {
        // mark
        for root in roots {
            root.mark_trace();
        }
        
        // sweep
        unsafe { self.sweep(); }
    }
    
    unsafe fn sweep(&mut self) {
        let _guard = DropGuard::new();
        
        //boxes_start: Option<NonNull<GCBox<dyn GCTrace>>>,
        let mut prev_box = None;
        let mut next_box = self.boxes_start;
        while let Some(gcbox) = next_box {
            let box_ptr = gcbox.as_ptr();
            
            if (*box_ptr).marked {
                (*box_ptr).marked = false;
                prev_box.replace(gcbox);
                
                next_box = (*box_ptr).next;
            } else {
                next_box = self.free(gcbox)
            }
        }
    }
}

impl Drop for GCState {
    fn drop(&mut self) {
        // unimplemented!()
    }
}


// Whether or not the thread is currently in the sweep phase of garbage collection.
thread_local!(pub static GC_SWEEP: Cell<bool> = Cell::new(false));

struct DropGuard;

impl DropGuard {
    fn new() -> DropGuard {
        GC_SWEEP.with(|flag| flag.set(true));
        DropGuard
    }
}

impl Drop for DropGuard {
    fn drop(&mut self) {
        GC_SWEEP.with(|flag| flag.set(false));
    }
}

fn deref_safe() -> bool {
    GC_SWEEP.with(|flag| !flag.get())
}