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


pub struct Upvalue {
    index: usize,  // index into the value stack
}


#[derive(Debug)]
pub struct Function {
    signature: Signature,
    module: GC<Module>,
    chunk_id: ChunkID,
}

impl GCTrace for Function { }

impl Function {
    pub fn new(signature: Signature, module: GC<Module>, chunk_id :ChunkID) -> Self {
        Self { signature, module, chunk_id }
    }
}

impl Invoke for Function {
    fn signature(&self) -> &Signature { &self.signature }
    
    fn as_call(&self) -> Call {
        Call::Chunk(self.module, self.chunk_id)
    }
}


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


