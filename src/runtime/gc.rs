use core::fmt;
use core::ptr::NonNull;
use core::cell::{Cell, RefCell};
use log;

mod data;
mod handle;
mod weak;

pub use data::GcTrace;
pub use handle::{Gc, GcWeak};

use data::{GcBox, GcBoxPtr};


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
    boxes_start: Option<GcBoxPtr>,
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
    
    fn insert<T>(&mut self, mut gcbox: NonNull<GcBox<T>>) where T: GcTrace + ?Sized {
        unsafe {
            let size = gcbox.as_ref().header().size();
            log::debug!("{:#X} allocate {} bytes", gcbox.as_ptr() as *const () as usize, size);
            
            gcbox.as_mut().header_mut().set_next(self.boxes_start.take());
            self.boxes_start = Some(GcBoxPtr::new(gcbox));
            self.stats.allocated += size;
            self.stats.box_count += 1;
        }
    }
    
    /// frees the GcBox, yielding it's next pointer
    fn free(&mut self, gcbox: GcBoxPtr) -> Option<GcBoxPtr> {
        let size = unsafe { gcbox.header().size() };
        self.stats.allocated -= size;
        self.stats.box_count -= 1;
        log::debug!("{:#X} free {} bytes", gcbox.as_ptr() as *const () as usize, size);
        
        unsafe { gcbox.free() }
    }
    
    fn collect_garbage(&mut self, root: &impl GcTrace) {
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
        
        let mut prev_box = None;
        let mut next_box = self.boxes_start;
        while let Some(mut gcbox) = next_box {
            if gcbox.header().is_marked() {
                gcbox.header_mut().set_marked(false);
                
                next_box = gcbox.header().next();
                prev_box.replace(gcbox);
                
            } else {
                
                next_box = self.free(gcbox);
                if let Some(mut prev_box) = prev_box {
                    prev_box.header_mut().set_next(next_box);
                } else {
                    self.boxes_start = next_box;
                }
                
            }
        }
    }
}

impl Drop for GcState {
    fn drop(&mut self) {
        unsafe { self.sweep() }
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
