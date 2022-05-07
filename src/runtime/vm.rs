use core::cell::Cell;
use core::ops::Deref;
use crate::codegen::LocalIndex;
use crate::runtime::{Variant, HashMap};
use crate::runtime::gc::{Gc, GcWeak, GcTrace, gc_collect};
use crate::runtime::function::{Call, Function, Upvalue, UpvalueIndex, Closure};
use crate::runtime::module::Module;
use crate::runtime::errors::ExecResult;
use crate::debug::traceback::TraceSite;
use crate::debug::snapshot::{VMSnapshot, VMFrameSnapshot};

mod callframe;
mod instruction;

use callframe::VMCallFrame;


pub const SYSTEM_ARGS: usize = 2; // [ callee, nargs, ... ]

// Helpers

// data used to set up a new call
struct CallInfo {
    frame: usize,
    call: Call,
    site: TraceSite,
}

enum Control {
    Next,            // keep executing
    Call(CallInfo),  // setup a call
    Return(Variant), // return from call
    Exit(Variant),   // stop execution
}


#[derive(Debug, Clone, Copy)]
struct UpvalueRef {
    fun: Gc<Function>,
    index: UpvalueIndex,
}

impl UpvalueRef {
    fn mark_trace(&self) {
        self.fun.mark_trace()
    }
    
    fn weakref(&self) -> UpvalueWeakRef {
        UpvalueWeakRef {
            fun: self.fun.weakref(),
            index: self.index,
        }
    }
}

impl Deref for UpvalueRef {
    type Target = Upvalue;
    
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.fun.upvalues()[usize::from(self.index)]
    }
}

impl Gc<Function> {
    #[inline(always)]
    fn upvalue(self, index: UpvalueIndex) -> UpvalueRef {
        UpvalueRef {
            fun: self,
            index
        }
    }
}


#[derive(Debug, Clone, Copy)]
struct UpvalueWeakRef {
    fun: GcWeak<Function>,
    index: UpvalueIndex,
}

impl UpvalueWeakRef {
    #[inline(always)]
    fn try_deref(&self) -> Option<&Upvalue> {
        self.fun.try_deref()
            .map(|fun| &fun.upvalues()[usize::from(self.index)])
    }
    
    fn is_valid(&self) -> bool {
        self.fun.is_valid()
    }
    
    fn mark_trace(&self) {
        self.fun.mark_trace()
    }
}




// struct UpvalueWeakRef


// Stack-based Virtual Machine
#[derive(Debug)]
pub struct VirtualMachine<'c> {
    traceback: Vec<TraceSite>,
    
    frame: VMCallFrame<'c>,  // the active call frame
    calls: Vec<VMCallFrame<'c>>,
    values: ValueStack,
    upvalues: OpenUpvalues,
}

impl<'c> VirtualMachine<'c> {
    /// Create a new VM with the specified root module and an empty main chunk
    pub fn new(main_module: Gc<Module>, main_chunk: &'c [u8]) -> Self {
        Self {
            traceback: Vec::new(),
            calls: Vec::new(),
            values: ValueStack::new(),
            frame: VMCallFrame::main_chunk(main_module, main_chunk),
            upvalues: OpenUpvalues::new(),
        }
    }
    
    pub fn frame(&self) -> &VMCallFrame<'_> { &self.frame }
    
    // the return value is mostly of interest to the REPL
    pub fn run(mut self) -> ExecResult<Variant> {
        loop {
            if let Control::Exit(value) = self.exec_next()? {
                return Ok(value)
            }
        }
    }
    
    pub fn run_steps(self) -> impl Iterator<Item=ExecResult<VMSnapshot>> + 'c {
        VMStepper::from(self)
    }
    
    #[inline]
    fn exec_next(&mut self) -> ExecResult<Control> {
        let control = self.frame.exec_next(&mut self.values, &mut self.upvalues)
            .map_err(|error| error.extend_trace(self.traceback.iter().rev().cloned()))?;
        
        match &control {
            Control::Exit(..) => return Ok(control),
            
            Control::Return(value) if self.calls.is_empty() =>
                return Ok(Control::Exit(*value)),
            
            Control::Return(value) => self.return_call(*value),
            Control::Call(info) => self.setup_call(info)?,
            
            Control::Next => { }
        }
        
        self.upvalues.prune_invalid();
        gc_collect(self);
        
        Ok(control)
    }
    
    fn setup_call(&mut self, callinfo: &CallInfo) -> ExecResult<()> {
        self.traceback.push(callinfo.site.clone());
        
        match callinfo.call {
            Call::Native { func, nargs } => {
                let args = self.values.peek_many(nargs)
                    .iter().copied().collect::<Vec<Variant>>();
                
                let retval = func.exec_fun(self, &args)?;
                self.values.truncate(callinfo.frame);
                self.values.push(retval);
                self.traceback.pop();
            },
            
            Call::Chunk { module, chunk_id, nparams } => {
                let locals = LocalIndex::try_from(SYSTEM_ARGS + nparams)
                    .expect("local index overflow");
                
                let mut frame = VMCallFrame::call_frame(module, chunk_id, callinfo.frame, locals);
                core::mem::swap(&mut self.frame, &mut frame);
                self.calls.push(frame);
                
                log::debug!("Setup call: {{ frame: {}, locals: {} }}", self.frame.start_index(), self.frame.locals());
                log::debug!("Stack: {:?}", self.values);
            },
        }
        
        Ok(())
    }
    
    fn return_call(&mut self, retval: Variant) {
        let frame_idx = self.frame.start_index();
        
        let mut frame = self.calls.pop().expect("empty call stack");
        core::mem::swap(&mut self.frame, &mut frame);
        
        self.values.truncate(frame_idx);
        self.values.push(retval);
        self.traceback.pop();
        
        log::debug!("Return call: {{ frame: {}, locals: {} }}", self.frame.start_index(), self.frame.locals());
        log::debug!("Stack: {:?}", self.values);
    }
}

// trace through all Gc roots
unsafe impl GcTrace for VirtualMachine<'_> {
    fn trace(&self) {
        // trace through the value stack
        for value in self.values.stack.iter() {
            value.trace();
        }
        
        // trace through the active frame and all calls
        self.frame.trace();
        for frame in self.calls.iter() {
            frame.trace();
        }
        
        // traceback
        for site in self.traceback.iter() {
            site.trace();
        }
        
        // open upvalues
        for upval_ref in self.upvalues.iter_refs() {
            upval_ref.mark_trace();
        }
    }
}


// Stack Manipulation
#[derive(Debug)]
struct ValueStack {
    stack: Vec<Variant>,
}

impl ValueStack {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }
    
    fn take(self) -> Vec<Variant> {
        self.stack
    }
    
    // Note: when moving values off the stack, make sure to copy *before* popping
    // This ensures that the Gc sees the values as rooted
    
    #[inline(always)]
    fn len(&self) -> usize {
        self.stack.len()
    }
    
    #[inline(always)]
    fn is_empty(&self) -> bool {
        self.stack.len() == 0
    }
    
    #[inline(always)]
    fn clear(&mut self) {
        self.stack.clear()
    }
    
    #[inline(always)]
    fn pop(&mut self) -> Variant {
        self.stack.pop().expect("empty stack")
    }
    
    #[inline(always)]
    fn pop_many(&mut self, count: usize) -> Vec<Variant> {
        self.stack.split_off(self.stack.len() - count)
    }

    #[inline(always)]
    fn truncate(&mut self, index: usize) {
        self.stack.truncate(index)
    }
    
    #[inline(always)]
    fn discard(&mut self, count: usize) {
        self.stack.truncate(self.stack.len() - count)
    }
    
    #[inline(always)]
    fn discard_at(&mut self, index: usize, count: usize) {
        let discard_range = index..(index + count);
        self.stack.splice(discard_range, core::iter::empty());
    }
    
    #[inline(always)]
    fn push(&mut self, value: Variant) {
        self.stack.push(value)
    }
    
    #[inline(always)]
    fn insert(&mut self, index: usize, value: Variant) {
        self.stack.insert(index, value);
    }
    
    #[inline(always)]
    fn replace(&mut self, value: Variant) {
        *self.stack.last_mut().expect("empty stack") = value;
    }
    
    #[inline(always)]
    fn replace_many(&mut self, count: usize, value: Variant) {
        let replace_range = (self.stack.len()-count).. ;
        self.stack.splice(replace_range, core::iter::once(value));
    }
    
    #[inline(always)]
    fn replace_at(&mut self, index: usize, value: Variant) {
        let item = self.stack.get_mut(index)
            .expect("value index out of bounds");
        *item = value;
    }
    
    #[inline(always)]
    fn swap_last(&mut self, index: usize) {
        let len = self.stack.len();
        self.stack.swap(index, len - 1)
    }
    
    #[inline(always)]
    fn peek(&self) -> &Variant {
        self.stack.last().expect("empty stack")
    }
    
    #[inline(always)]
    fn peek_at(&self, index: usize) -> &Variant {
        self.stack.get(index)
            .expect("value index out of bounds")
    }
    
    #[inline(always)]
    fn peek_many(&self, count: usize) -> &[Variant] {
        let (_, peek) = self.stack.as_slice().split_at(self.stack.len() - count);
        peek
    }
    
    #[inline]
    fn get_closure(&self, closure: &Closure) -> Variant {
        match closure {
            Closure::Open(index) => *self.peek_at(*index),
            Closure::Closed(cell) => cell.get(),
        }
    }
    
    #[inline]
    fn set_closure(&mut self, closure: &Closure, value: Variant) {
        match closure {
            Closure::Open(index) => self.replace_at(*index, value),
            Closure::Closed(cell) => cell.set(value),
        }
    }
}

// Tracking open upvalues

#[derive(Debug)]
struct OpenUpvalues {
    upvalues: HashMap<usize, Vec<UpvalueWeakRef>>,
}

impl OpenUpvalues {
    fn new() -> Self {
        Self {
            upvalues: HashMap::with_hasher(Default::default())
        }
    }
    
    fn iter_refs(&self) -> impl Iterator<Item=&UpvalueWeakRef> {
        self.upvalues.values().flat_map(|refs| refs.iter())
    }
    
    fn prune_invalid(&mut self) {
        self.upvalues.retain(|_, upvals| {
            upvals.retain(UpvalueWeakRef::is_valid);
            !upvals.is_empty()
        })
    }
    
    /// Enter all of a function's upvalues into the upvalue registry
    fn register(&mut self, function: Gc<Function>) {
        for (index, upval) in function.upvalues().iter().enumerate() {
            if upval.closure().is_open() {
                let upval_ref = function.upvalue(index.try_into().unwrap());
                self.insert_ref(upval_ref);
            }
        }
    }
    
    fn insert_ref(&mut self, upval_ref: UpvalueRef) {
        let index = match upval_ref.closure() {
            Closure::Open(index) => index,
            _ => panic!("insert non-open upvalue"),
        };
        
        let weak_ref = upval_ref.weakref();
        self.upvalues.entry(index)
            .and_modify(|refs| refs.push(weak_ref))
            .or_insert_with(|| vec![ weak_ref ]);
    }
    
    fn close_upvalues(&mut self, index: usize, value: Variant) {
        if let Some(upvalues) = self.upvalues.remove(&index) {
            let gc_cell = Gc::new(Cell::new(value));
            for weak_ref in upvalues.iter() {
                if let Some(upvalue) = weak_ref.try_deref() {
                    upvalue.close(gc_cell)
                }
            }
        }
    }
}


// For debugging


impl From<&VirtualMachine<'_>> for VMSnapshot {
    fn from (vm: &VirtualMachine) -> Self {
        Self {
            calls: vm.calls.iter().map(VMFrameSnapshot::from).collect(),
            values: vm.values.stack.clone(),
            frame: (&vm.frame).into(),
        }
    }
}


struct VMStepper<'m> {
    vm: VirtualMachine<'m>,
    start: bool,
    stop: bool,
}

impl<'m> From<VirtualMachine<'m>> for VMStepper<'m> {
    fn from(vm: VirtualMachine<'m>) -> Self {
        Self {
            vm, 
            start: false,
            stop: false,
        }
    }
}

impl<'m> Iterator for VMStepper<'m> {
    type Item = ExecResult<VMSnapshot>;
    
    fn next(&mut self) -> Option<Self::Item> {
        if !self.start {
            self.start = true;
            return Some(Ok(VMSnapshot::from(&self.vm)))
        }
        
        if !self.stop {
            let status = self.vm.exec_next();
            if let Err(error) = status {
                self.stop = true;
                return Some(Err(error));
            }
            if let Ok(Control::Exit(..)) = status {
                self.stop = true;
            }
            return Some(Ok(VMSnapshot::from(&self.vm)))
        }
        
        None
    }
}