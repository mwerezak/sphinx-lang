///! The current implementation is fairly primitive, and will probably change

use std::cell::{RefCell, Ref, RefMut};
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;

use crate::runtime::types::function::Function;


thread_local! {
    pub static GC_STATE: RefCell<GCState> = RefCell::new(GCState::init());
}


pub enum GCObject {
    Function(Box<Function>),
    // Object(Box<...>),
    // Metatable(Box<...>),
    
}

impl GCObject {
    pub fn allocate(self) -> GCHandle {
        GC_STATE.with(|gc| gc.borrow_mut().insert(self))
    }
}

type PhantomUnsend = PhantomData<*mut ()>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct GCHandle {
    index: usize,
    _marker: PhantomUnsend,
}

impl From<usize> for GCHandle {
    fn from(index: usize) -> Self {
        Self { index, _marker: PhantomData }
    }
}

impl GCHandle {
    fn index(&self) -> usize { self.index }
    
    pub fn with_ref<R>(&self, func: impl FnOnce(Ref<GCObject>) -> R) -> R {
        GC_STATE.with(|gc| func(gc.borrow().lookup(self)))
    }
    
    pub fn with_mut<R>(&self, func: impl FnOnce(RefMut<GCObject>) -> R) -> R {
        GC_STATE.with(|gc| func(gc.borrow().lookup_mut(self)))
    }
}

impl std::fmt::Debug for GCHandle {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "GC(&{})", self.index)
    }
}


struct GCBox {
    marked: bool,
    object: RefCell<GCObject>,
}

impl GCBox {
    fn new(object: GCObject) -> Box<Self> {
        let item = Self {
            marked: false,
            object: RefCell::new(object),
        };
        Box::new(item)
    }
    
    fn borrow(&self) -> Ref<GCObject> {
        self.object.borrow()
    }
    
    fn borrow_mut(&self) -> RefMut<GCObject> {
        self.object.borrow_mut()
    }
}


pub struct GCState {
    // stats: 
    // config: 
    boxes: Vec<Box<GCBox>>,
}

impl GCState {
    fn init() -> Self {
        Self {
            boxes: Vec::new(),
        }
    }
    
    pub fn insert(&mut self, object: GCObject) -> GCHandle {
        let index = self.boxes.len();
        self.boxes.push(GCBox::new(object));
        GCHandle::from(index)
    }
    
    pub fn lookup(&self, handle: &GCHandle) -> Ref<GCObject> {
        let gcbox = self.boxes.get(handle.index()).expect("invalid handle");
        gcbox.borrow()
    }
    
    pub fn lookup_mut(&self, handle: &GCHandle) -> RefMut<GCObject> {
        let gcbox = self.boxes.get(handle.index()).expect("invalid handle");
        gcbox.borrow_mut()
    }
}