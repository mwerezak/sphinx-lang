use std::cell::{RefCell, Ref, RefMut, Cell};
use std::ops::Deref;
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
    
    pub fn get_upvalue(&self, index: UpvalueIndex) -> Ref<Upvalue> {
        Ref::map(self.upvalues.borrow(), |upvals| &upvals[usize::from(index)])
    }
    
    pub fn insert_upvalue(&self, upvalue: Upvalue) {
        self.upvalues.borrow_mut().push(upvalue);
    }
}

impl Invoke for Function {
    fn signature(&self) -> &Signature { &self.signature }
    
    #[inline]
    fn as_call(&self) -> Call {
        Call::Chunk(self.module, self.chunk_id)
    }
}

/// References an upvalue in a GC'd function
#[derive(Debug, Clone, Copy)]
pub struct UpvalueRef {
    fun: GC<Function>,
    index: UpvalueIndex,
}

impl UpvalueRef {
    #[inline(always)]
    pub fn borrow(&self) -> Ref<Upvalue> {
        self.fun.get_upvalue(self.index)
    }
}

impl GC<Function> {
    #[inline]
    pub fn ref_upvalue(self, index: UpvalueIndex) -> UpvalueRef {
        UpvalueRef {
            fun: self,
            index
        }
    }
}


// Closures

pub type UpvalueIndex = u16;

#[derive(Debug, Clone, Copy)]
pub enum Closure {
    Open(usize),
    Closed(GC<Cell<Variant>>),
}


#[derive(Debug, Clone)]
pub struct Upvalue {
    value: Cell<Closure>,
}

impl Upvalue {
    pub fn new(index: usize) -> Self {
        Self {
            value: Cell::new(Closure::Open(index)),
        }
    }
    
    #[inline]
    pub fn value(&self) -> Closure { self.value.get() }
    
    #[inline]
    pub fn close(&self, value_cell: GC<Cell<Variant>>) {
        self.value.set(Closure::Closed(value_cell))
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
    
    #[inline]
    fn as_call(&self) -> Call {
        Call::Native(*self)
    }
}


