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
}

pub trait GCTrace {
    /// If the GC'd data owns any allocations, this should return the extra allocated size.
    /// This is only called once when ownership is taken by the GC, and again when dropped,
    /// so consistency is more important than accuracy when dealing with mutable data that
    /// can grow in size.
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