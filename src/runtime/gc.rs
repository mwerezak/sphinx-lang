/// Partial clone of rust-gc's mark and sweep implementation

use std::mem;
use std::any::Any;
use std::ptr::NonNull;
use std::cell::{Cell, RefCell};
use log;

mod handle;
pub use handle::GCHandle;


thread_local! {
    static GC_STATE: RefCell<GCState> = RefCell::new(GCState::default());
}

struct GCState {
    stats: GCStats,
    config: GCConfig,
    boxes_start: Option<NonNull<GCBox<dyn Any>>>,
}

/// TODO store GCBoxes in linear chunks instead of individual linked nodes
struct GCBox<T> where T: Any + ?Sized + 'static {
    next: Option<NonNull<GCBox<dyn Any>>>,
    marked: bool,
    data: T,
}

impl<T> GCBox<T> {
    fn value(&self) -> &T { &self.data }
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
            threshold: 8*1024, // 8 kiB
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
    
    fn allocate<T>(&mut self, data: T) -> NonNull<GCBox<T>> {
        if self.should_collect() {
            unimplemented!()
        }
        
        let gcbox = Box::new(GCBox {
            next: None,
            marked: false,
            data,
        });
        
        self.insert_box(gcbox)
    }
    
    fn insert_box<T>(&mut self, mut gcbox: Box<GCBox<T>>) -> NonNull<GCBox<T>> {
        gcbox.next = self.boxes_start.take();
        
        let ptr = unsafe {
            NonNull::new_unchecked(Box::into_raw(gcbox))
        };
        self.boxes_start = Some(ptr);
        self.stats.allocated +=  mem::size_of::<GCBox<T>>();
        
        ptr
    }
    
    
}

impl Drop for GCState {
    fn drop(&mut self) {
        unimplemented!()
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