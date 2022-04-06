use std::cell::{RefCell, Ref, Cell};
use crate::codegen::FunctionID;
use crate::runtime::Variant;
use crate::runtime::module::Module;
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::errors::ExecResult;

mod signature;

pub use signature::{Signature, Parameter};
pub use crate::codegen::opcodes::UpvalueIndex;


/// Call directive


pub enum Call {
    Chunk(GC<Module>, FunctionID),
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
    fun_id: FunctionID,
    upvalues: Box<[Upvalue]>,
}

impl GCTrace for Function { }

impl Function {
    pub fn new(signature: Signature, module: GC<Module>, fun_id: FunctionID, upvalues: Box<[Upvalue]>) -> Self {
        Self { 
            signature, 
            module, 
            fun_id,
            upvalues,
        }
    }
    
    pub fn upvalues(&self) -> &[Upvalue] { &self.upvalues }
}

impl Invoke for Function {
    fn signature(&self) -> &Signature { &self.signature }
    
    #[inline]
    fn as_call(&self) -> Call {
        Call::Chunk(self.module, self.fun_id)
    }
}


// Closures

#[derive(Debug, Clone, Copy)]
pub enum Closure {
    Open(usize),
    Closed(GC<Cell<Variant>>),
}

impl Closure {
    pub fn is_open(&self) -> bool { matches!(self, Self::Open(..)) }
    pub fn is_closed(&self) -> bool { matches!(self, Self::Closed(..)) }
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
    pub fn close(&self, gc_cell: GC<Cell<Variant>>) {
        self.value.set(Closure::Closed(gc_cell))
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


