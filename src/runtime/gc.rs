/// Partial clone of rust-gc's mark and sweep implementation

use core::fmt;
use core::mem;
use core::ptr::{self, NonNull};
use core::cell::{Cell, RefCell};
use log;

mod data;
mod handle;

pub use data::GCTrace;
pub use handle::GC;

use data::GCBox;


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
        let gcbox = GCBox::new(data);
        let ptr = gcbox.as_ptr();

        unsafe {
            let size = (*ptr).size();
            log::debug!("{:#X} allocate {} bytes", ptr as usize, size);
            
            (*ptr).set_next(self.boxes_start.take());
            self.boxes_start = Some(gcbox);
            self.stats.allocated += size;
            self.stats.box_count += 1;
        }
        
        gcbox
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
        
        gcbox.next()
        
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