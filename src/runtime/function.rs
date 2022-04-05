use std::cell::{RefCell, Ref, RefMut, Cell};
use crate::codegen::{ChunkID, ConstID};
use crate::runtime::Variant;
use crate::runtime::module::{Module, Access};
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::errors::ExecResult;

mod signature;

pub use signature::{Signature, Parameter};


/// Call directive


pub enum Call {
    Chunk(GC<Module>, ChunkID),
    Native(GC<NativeFunction>),
}

pub trait Invoke {
    fn signature(&self) -> &Signature;
    fn as_call(&self) -> Call;
    
    fn invoke(&self, args: &[Variant]) -> ExecResult<Call> {
        self.signature().check_args(args)?;
        Ok(self.as_call())
    }
}


// Compiled Functions

#[derive(Debug)]
pub struct Function {
    signature: Signature,
    module: GC<Module>,
    chunk_id: ChunkID,
    upvalues: RefCell<Vec<Upvalue>>,
}

impl GCTrace for Function { }

impl Function {
    pub fn new(signature: Signature, module: GC<Module>, chunk_id :ChunkID) -> Self {
        Self { 
            signature, 
            module, 
            chunk_id,
            upvalues: RefCell::new(Vec::new()),
        }
    }
    
    pub fn upvalues(&self) -> Ref<[Upvalue]> {
        Ref::map(self.upvalues.borrow(), |upvalues| upvalues.as_slice())
    }
    
    pub fn upvalues_mut(&self) -> RefMut<[Upvalue]> {
        RefMut::map(self.upvalues.borrow_mut(), |upvalues| upvalues.as_mut_slice())
    }
    
    pub fn insert_upvalue(&self, upvalue: Upvalue) {
        self.upvalues.borrow_mut().push(upvalue);
    }
}

impl Invoke for Function {
    fn signature(&self) -> &Signature { &self.signature }
    
    fn as_call(&self) -> Call {
        Call::Chunk(self.module, self.chunk_id)
    }
}


// Closures

pub type UpvalueIndex = u16;

#[derive(Debug, Clone, Copy)]
pub enum Closure {
    Open(usize),
    Closed(GC<Cell<Variant>>),
}


// note: the indirection is necessary because otherwise
// we would have to heap allocate upvalues and store them behind an Rc,
// or we would have to track every copy that was made from an upvalue
#[derive(Debug, Clone)]
pub enum Upvalue {
    Local(Cell<Closure>),
    Extern(GC<Function>, UpvalueIndex), 
}

impl Upvalue {
    pub fn new_local(index: usize) -> Self {
        Self::Local(Cell::new(Closure::Open(index)))
    }
    
    pub fn new_extern(owner: GC<Function>, index: UpvalueIndex) -> Self {
        Self::Extern(owner, index)
    }
    
    /// Retrieve the closure for this upvalue, dereferencing Externs as needed
    #[inline]
    pub fn closure(&self) -> Closure {
        match self {
            Upvalue::Local(closure) => 
                return closure.get(),
            
            // TODO non-recursive implementation?
            Upvalue::Extern(fun, idx) => 
                return fun.upvalues()[usize::from(*idx)].closure(),
        }
    }
    
    #[inline]
    pub fn close(&self, value: Variant) {
        match self {
            Upvalue::Extern(..) => panic!("close extern upvalue"),
            
            Upvalue::Local(closure) => {
                let cell = GC::allocate(Cell::new(value));
                closure.set(Closure::Closed(cell));
            }
        }
    }
}



// Native Functions

pub type NativeFn = fn(self_fun: &NativeFunction, args: &[Variant]) -> ExecResult<Variant>;

pub struct NativeFunction {
    signature: Signature,
    defaults: Option<Box<[Variant]>>,
    func: NativeFn,
}

impl GCTrace for NativeFunction { }

impl NativeFunction {
    pub fn new(signature: Signature, defaults: Option<Box<[Variant]>>, func: NativeFn) -> Self {
        Self { signature, defaults, func }
    }
    
    pub fn signature(&self) -> &Signature { &self.signature }
    
    pub fn defaults(&self) -> &[Variant] {
        match self.defaults.as_ref() {
            Some(defaults) => &*defaults,
            None => &[],
        }
    }
    
    pub fn invoke(&self, args: &[Variant]) -> ExecResult<Variant> {
        (self.func)(self, args)
    }
}

impl Invoke for GC<NativeFunction> {
    fn signature(&self) -> &Signature { &self.signature }
    
    fn as_call(&self) -> Call {
        Call::Native(*self)
    }
}


