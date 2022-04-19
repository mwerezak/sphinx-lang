/// Partial clone of rust-gc's mark and sweep implementation

use core::fmt;
use core::mem;
use core::ptr::{self, NonNull};
use core::cell::{Cell, RefCell};
use log;

mod data;
mod handle;

pub use data::GcTrace;
pub use handle::Gc;

use data::GcBox;


thread_local! {
    static GC_STATE: RefCell<GcState> = RefCell::new(GcState::default());
}

pub fn gc_collect(root: &impl GcTrace) {
    GC_STATE.with(|gc| {
        let mut gc = gc.borrow_mut();
        if gc.should_collect() {
            gc.collect_garbage(root)
        }
    })
}

pub fn gc_force(root: &impl GcTrace) {
    GC_STATE.with(|gc| {
        let mut gc = gc.borrow_mut();
        gc.collect_garbage(root)
    })
}

struct GcState {
    stats: GcStats,
    config: GcConfig,
    threshold: usize,
    boxes_start: Option<NonNull<GcBox<dyn GcTrace>>>,
}

#[derive(Debug)]
struct GcStats {
    allocated: usize,
    box_count: usize,
    cycle_count: usize,
}

struct GcConfig {
    threshold: u16,
    pause_factor: u16,  // percent memory use relative to last cycle before starting a new cycle
}

impl Default for GcConfig {
    fn default() -> Self {
        Self {
            threshold: 512, // Small because of stop-the-world. If we go incremental increase this to 8 kiB
            pause_factor: 160,
        }
    }
}

impl Default for GcState {
    fn default() -> Self {
        GcState::new(GcConfig::default())
    }
}

impl GcState {
    fn new(config: GcConfig) -> Self {
        let threshold = config.threshold as usize;
        
        Self {
            config,
            threshold,
            
            stats: GcStats {
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
    
    fn insert(&mut self, gcbox: NonNull<GcBox<dyn GcTrace>>) {
        unsafe {
            let ptr = gcbox.as_ptr();
            let size = (*ptr).size();
            log::debug!("{:#X} allocate {} bytes", ptr as *const () as usize, size);
            
            (*ptr).set_next(self.boxes_start.take());
            self.boxes_start = Some(gcbox);
            self.stats.allocated += size;
            self.stats.box_count += 1;
        }
    }
    
    /// frees the GcBox, yielding it's next pointer
    fn free(&mut self, gcbox: NonNull<GcBox<dyn GcTrace>>) -> Option<NonNull<GcBox<dyn GcTrace>>> {
        let gcbox = gcbox.as_ptr();
        
        let size = unsafe { (*gcbox).size() };
        self.stats.allocated -= size;
        self.stats.box_count -= 1;
        log::debug!("{:#X} free {} bytes", gcbox as *const () as usize, size);
        
        unsafe { GcBox::free(gcbox) }
    }
    
    fn collect_garbage(&mut self, root: &impl GcTrace) {
        log::debug!("Gc cycle begin ---");
        
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
        
        log::debug!("Gc cycle end ---");
    }
    
    unsafe fn sweep(&mut self) {
        let _guard = DropGuard::new();
        
        //boxes_start: Option<NonNull<GcBox<dyn GcTrace>>>,
        let mut prev_box = None;
        let mut next_box = self.boxes_start;
        while let Some(gcbox) = next_box {
            let box_ptr = gcbox.as_ptr();
            
            if (*box_ptr).is_marked() {
                (*box_ptr).clear_mark();
                
                next_box = (*box_ptr).next();
                prev_box.replace(gcbox);
                
            } else {
                
                next_box = self.free(gcbox);
                if let Some(prev_box) = prev_box {
                    (*prev_box.as_ptr()).set_next(next_box);
                } else {
                    self.boxes_start = next_box;
                }
                
            }
        }
    }
}

impl Drop for GcState {
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


impl fmt::Display for GcStats {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt, "Cycle {}: estimated usage {}u ({} allocations)", 
            self.cycle_count, self.allocated, self.box_count
        )
    }
}