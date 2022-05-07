use core::fmt;
use core::cell::Cell;
use crate::codegen::{FunctionID, FunctionProto};
use crate::runtime::Variant;
use crate::runtime::module::{Module, GlobalEnv};
use crate::runtime::vm::VirtualMachine;
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::errors::ExecResult;

mod signature;

pub use signature::{Signature, Parameter};
pub use crate::codegen::opcodes::UpvalueIndex;


/// Call directive

pub enum Call {
    Chunk {
        module: Gc<Module>,
        chunk_id: FunctionID,
        fixed_nargs: usize,
    },
    Native {
        func: Gc<NativeFunction>,
        nargs: usize,
    },
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
    module: Gc<Module>,
    upvalues: Box<[Upvalue]>,
}

impl Function {
    pub fn new(fun_id: FunctionID, module: Gc<Module>, upvalues: Box<[Upvalue]>) -> Self {
        Self { fun_id, module, upvalues }
    }
    
    pub fn upvalues(&self) -> &[Upvalue] { &self.upvalues }
    
    pub fn proto(&self) -> &FunctionProto {
        self.module.data().get_function(self.fun_id)
    }
    
    pub fn signature(&self) -> &Signature {
        self.proto().signature()
    }
    
    fn fixed_args_count(&self) -> usize {
        let signature = self.signature();
        
        signature.required().len()
        + signature.default().len()
        + if signature.variadic().is_some() { 1 } else { 0 }
    }
}

impl Callable for Function {
    fn signature(&self) -> &Signature { self.proto().signature() }
    
    fn raw_call(&self, _args: &[Variant]) -> Call {
        Call::Chunk {
            module: self.module,
            chunk_id: self.fun_id,
            fixed_nargs: self.fixed_args_count(),
        }
    }
}

impl Callable for Gc<Function> {
    fn signature(&self) -> &Signature {
        <Function as Callable>::signature(self)
    }
    
    fn raw_call(&self, args: &[Variant]) -> Call {
        <Function as Callable>::raw_call(self, args)
    }
}

unsafe impl GcTrace for Function {
    fn trace(&self) {
        self.module.mark_trace();
        for upval in self.upvalues.iter() {
            if let Closure::Closed(gc_cell) = upval.closure() {
                gc_cell.mark_trace();
            }
        }
    }
    
    fn size_hint(&self) -> usize {
        core::mem::size_of::<Upvalue>() * self.upvalues.len()
    }
}


// Closures

#[derive(Debug, Clone, Copy)]
pub enum Closure {
    Open(usize),
    Closed(Gc<Cell<Variant>>),
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
    pub fn closure(&self) -> Closure { self.value.get() }
    
    #[inline]
    pub fn close(&self, gc_cell: Gc<Cell<Variant>>) {
        self.value.set(Closure::Closed(gc_cell))
    }
}



// Native Functions

pub type NativeFn = fn(self_fun: &NativeFunction, vm: &mut VirtualMachine<'_>, args: &[Variant]) -> ExecResult<Variant>;

pub struct NativeFunction {
    signature: Signature,
    defaults: Option<Box<[Variant]>>,
    env: Gc<GlobalEnv>,
    func: NativeFn,
}

impl NativeFunction {
    pub fn new(signature: Signature, defaults: Option<Box<[Variant]>>, env: Gc<GlobalEnv>, func: NativeFn) -> Self {
        Self { signature, defaults, env, func }
    }
    
    pub fn signature(&self) -> &Signature { &self.signature }
    
    pub fn env(&self) -> Gc<GlobalEnv> { self.env }
    
    pub fn defaults(&self) -> &[Variant] {
        match self.defaults.as_ref() {
            Some(defaults) => &*defaults,
            None => &[],
        }
    }
    
    /// actually execute a native function
    pub fn exec_fun(&self, vm: &mut VirtualMachine<'_>, args: &[Variant]) -> ExecResult<Variant> {
        self.signature().check_args(args)?;
        (self.func)(self, vm, args)
    }
}

impl Callable for Gc<NativeFunction> {
    fn signature(&self) -> &Signature { &self.signature }
    
    fn raw_call(&self, args: &[Variant]) -> Call {
        Call::Native {
            func: *self,
            nargs: args.len(),
        }
    }
}

unsafe impl GcTrace for NativeFunction {
    fn trace(&self) {
        self.env.mark_trace();
        
        if let Some(defaults) = self.defaults.as_ref() {
            defaults.trace()
        }
    }
    
    fn size_hint(&self) -> usize {
        self.defaults.as_ref()
        .map_or(0, |defaults| {
            core::mem::size_of::<Variant>() * defaults.len()
        })
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("NativeFunction")
            .field("signature", &self.signature)
            .field("defaults", &self.defaults)
            .field("env", &self.env)
            .field("func", &core::ptr::addr_of!(self.func))
            .finish()
    }
}
