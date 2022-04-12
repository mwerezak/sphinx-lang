/// Partial clone of rust-gc's mark and sweep implementation

use std::fmt;
use std::mem;
use std::ptr::{self, NonNull};
use std::cell::{Cell, RefCell};
use log;

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
        ptr::eq(&self.marked, &other.marked)
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

pub fn gc_collect(root: &impl GCTrace) {
    GC_STATE.with(|gc| {
        let mut gc = gc.borrow_mut();
        if gc.should_collect() {
            gc.collect_garbage(root)
        }
    })
}

pub fn gc_force(root: &impl GCTrace) {
    GC_STATE.with(|gc| {
        let mut gc = gc.borrow_mut();
        gc.collect_garbage(root)
    })
}

struct GCState {
    stats: GCStats,
    config: GCConfig,
    threshold: usize,
    boxes_start: Option<NonNull<GCBox<dyn GCTrace>>>,
}

#[derive(Debug)]
struct GCStats {
    allocated: usize,
    box_count: usize,
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
            pause_factor: 160,
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
        let threshold = config.threshold as usize;
        
        Self {
            config,
            threshold,
            
            stats: GCStats {
                allocated: 0,
                box_count: 0,
                cycle_count: 0,
            },
            
            boxes_start: None,
        }
    }
    
    #[inline]
    fn should_collect(&self) -> bool {
        self.stats.allocated > self.threshold
    }
    
    fn allocate<T: GCTrace>(&mut self, data: T) -> NonNull<GCBox<T>> {
        
        let gcbox = Box::new(GCBox {
            next: self.boxes_start.take(),
            marked: false,
            data,
        });

        let size = gcbox.size();
        let ptr = Box::into_raw(gcbox);
        log::debug!("{:#X} allocate {} bytes", ptr as usize, size);
        
        let ptr = unsafe {
            NonNull::new_unchecked(ptr)
        };
        
        self.boxes_start = Some(ptr);
        self.stats.allocated += size;
        self.stats.box_count += 1;
        
        ptr
    }
    
    /// frees the GCBox, yielding it's next pointer
    fn free(&mut self, gcbox: NonNull<GCBox<dyn GCTrace>>) -> Option<NonNull<GCBox<dyn GCTrace>>> {
        let ptr = gcbox.as_ptr();
        
        // SAFETY: This is safe as long as we only ever free() GCBoxes that were created by allocate()
        let gcbox = unsafe { Box::from_raw(ptr) };
        
        let size = gcbox.size();
        self.stats.allocated -= size;
        self.stats.box_count -= 1;
        log::debug!("{:#X} free {} bytes", ptr as *const () as usize, size);
        
        gcbox.next
        
        // gcbox should get dropped here
    }
    
    fn collect_garbage(&mut self, root: &impl GCTrace) {
        log::debug!("GC cycle begin ---");
        
        let allocated = self.stats.allocated;
        let box_count = self.stats.box_count;
        log::debug!("{}", self.stats);
        
        // mark
        root.trace();
        
        // sweep
        unsafe { self.sweep(); }
        self.stats.cycle_count = self.stats.cycle_count.wrapping_add(1);
        
        let freed = allocated - self.stats.allocated;
        let dropped = box_count - self.stats.box_count;
        log::debug!("Freed {} bytes ({} allocations)", freed, dropped);
        log::debug!("{}", self.stats);
        
        self.threshold = (self.stats.allocated * self.config.pause_factor as usize) / 100;
        log::debug!("Next collection at {} bytes", self.threshold);
        
        log::debug!("GC cycle end ---");
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
                
                next_box = (*box_ptr).next;
                prev_box.replace(gcbox);
                
            } else {
                
                next_box = self.free(gcbox);
                if let Some(prev_box) = prev_box {
                    (*prev_box.as_ptr()).next = next_box;
                } else {
                    self.boxes_start = next_box;
                }
                
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


impl fmt::Display for GCStats {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt, "Cycle {}: estimated usage {}u ({} allocations)", 
            self.cycle_count, self.allocated, self.box_count
        )
    }
}