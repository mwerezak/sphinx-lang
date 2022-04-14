use std::cell::{RefCell, Ref, Cell};
use crate::codegen::{FunctionID, FunctionProto};
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

pub trait Callable {
    fn signature(&self) -> &Signature;
    fn raw_call(&self, args: &[Variant]) -> Call;

    fn checked_call(&self, args: &[Variant]) -> ExecResult<Call> {
        self.signature().check_args(args)?;
        Ok(self.raw_call(args))
    }

}

// Compiled Functions

#[derive(Debug)]
pub struct Function {
    fun_id: FunctionID,
    module: GC<Module>,
    upvalues: Box<[Upvalue]>,
}

impl Function {
    pub fn new(fun_id: FunctionID, module: GC<Module>, upvalues: Box<[Upvalue]>) -> Self {
        Self { fun_id, module, upvalues }
    }
    
    pub fn upvalues(&self) -> &[Upvalue] { &self.upvalues }
    
    pub fn proto(&self) -> &FunctionProto {
        self.module.data().get_function(self.fun_id)
    }
    
    pub fn signature(&self) -> &Signature {
        self.proto().signature()
    }
}

impl Callable for Function {
    fn signature(&self) -> &Signature { self.proto().signature() }
    
    fn raw_call(&self, _args: &[Variant]) -> Call {
        Call::Chunk(self.module, self.fun_id)
    }
}

impl Callable for GC<Function> {
    fn signature(&self) -> &Signature {
        <Function as Callable>::signature(self)
    }
    
    fn raw_call(&self, args: &[Variant]) -> Call {
        <Function as Callable>::raw_call(self, args)
    }
}

unsafe impl GCTrace for Function {
    fn trace(&self) {
        self.module.mark_trace();
        for upval in self.upvalues.iter() {
            if let Closure::Closed(gc_cell) = upval.closure() {
                gc_cell.mark_trace();
            }
        }
    }
    
    fn size_hint(&self) -> usize {
        std::mem::size_of::<Upvalue>() * self.upvalues.len()
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

unsafe impl GCTrace for Cell<Variant> {
    fn trace(&self) {
        self.get().trace()
    }
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
    pub fn closure(&self) -> Closure { self.value.get() }
    
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
        self.signature().check_args(args)?;
        (self.func)(self, args)
    }
}

impl Callable for GC<NativeFunction> {
    fn signature(&self) -> &Signature { &self.signature }
    
    fn raw_call(&self, _args: &[Variant]) -> Call {
        Call::Native(*self)
    }
}

unsafe impl GCTrace for NativeFunction {
    fn trace(&self) {
        if let Some(defaults) = self.defaults.as_ref() {
            defaults.trace()
        }
    }
    
    fn size_hint(&self) -> usize {
        self.defaults.as_ref()
        .map_or(0, |defaults| {
            std::mem::size_of::<Variant>() * defaults.len()
        })
    }
}

