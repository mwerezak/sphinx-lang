use crate::runtime::{Variant, HashMap};
use crate::runtime::gc::GC;
use crate::runtime::function::{Call, Function, Upvalue, UpvalueIndex, Closure};
use crate::runtime::module::Module;
use crate::runtime::errors::ExecResult;
use crate::debug::traceback::TraceSite;
use crate::debug::snapshot::{VMSnapshot, VMFrameSnapshot};

mod callframe;
pub use callframe::LocalIndex;

use callframe::{VMCallFrame};

// Helpers

// data used to set up a new call
struct CallInfo {
    nargs: usize,
    frame: usize,
    call: Call,
    site: TraceSite,
}

enum Control {
    Continue,
    Call(CallInfo),
    Return,
    Exit,
}


#[derive(Debug, Clone, Copy)]
struct UpvalueRef {
    // TODO weak reference
    fun: GC<Function>,
    index: UpvalueIndex,
}

impl UpvalueRef {
    #[inline(always)]
    fn borrow(&self) -> impl std::ops::Deref<Target=Upvalue> + '_ {
        self.fun.get_upvalue(self.index)
    }
}

impl GC<Function> {
    #[inline(always)]
    fn ref_upvalue(self, index: UpvalueIndex) -> UpvalueRef {
        UpvalueRef {
            fun: self,
            index
        }
    }
    
    #[inline(always)]
    fn ref_insert_upvalue(self, upvalue: Upvalue) -> UpvalueRef {
        let idx = self.insert_upvalue(upvalue);
        self.ref_upvalue(idx)
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
    pub fn new(main_module: GC<Module>, main_chunk: &'c [u8]) -> Self {
        Self {
            traceback: Vec::new(),
            calls: Vec::new(),
            values: ValueStack::new(),
            frame: VMCallFrame::main_chunk(main_module, main_chunk),
            upvalues: OpenUpvalues::new(),
        }
    }
    
    pub fn run(mut self) -> ExecResult<()> {
        while !self.exec_next()? { }
        Ok(())
    }
    
    pub fn run_steps(self) -> impl Iterator<Item=ExecResult<VMSnapshot>> + 'c {
        VMStepper::from(self)
    }
    
    #[inline]
    fn exec_next(&mut self) -> ExecResult<bool> {
        let control = self.frame.exec_next(&mut self.values, &mut self.upvalues)
            .map_err(|error| error.extend_trace(self.traceback.iter().rev().cloned()))?;
        
        match control {
            Control::Exit => return Ok(true),
            Control::Return if self.calls.is_empty() => return Ok(true),
            
            Control::Call(info) => self.setup_call(info)?,
            Control::Return => self.return_call(),
            Control::Continue => { }
        }
        
        Ok(false)
    }
    
    fn setup_call(&mut self, call: CallInfo) -> ExecResult<()> {
        self.traceback.push(call.site);
        
        match call.call {
            Call::Native(func) => {
                let args = self.values.peek_many(call.nargs);
                let retval = func.invoke(args)?;
                self.values.truncate(call.frame.into());
                self.values.push(retval);
                self.traceback.pop();
            },
            
            Call::Chunk(module, chunk_id) => {
                let locals = LocalIndex::try_from(self.values.len() - call.frame)
                    .expect("local index overflow");
                
                let mut frame = VMCallFrame::call_frame(module, chunk_id, call.frame, locals);
                std::mem::swap(&mut self.frame, &mut frame);
                self.calls.push(frame);
                
                log::debug!("Setup call: {{ frame: {}, locals: {} }}", self.frame.start_index(), self.frame.locals());
                log::debug!("Stack: {:?}", self.values);
            },
        }
        
        Ok(())
    }
    
    fn return_call(&mut self) {
        let frame_idx = self.frame.start_index();
        
        let mut frame = self.calls.pop().expect("empty call stack");
        std::mem::swap(&mut self.frame, &mut frame);
        
        let retval = self.values.pop();
        self.values.truncate(frame_idx.into());
        self.values.push(retval);
        self.traceback.pop();
        
        log::debug!("Return call: {{ frame: {}, locals: {} }}", self.frame.start_index(), self.frame.locals());
        log::debug!("Stack: {:?}", self.values);
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
    // This ensures that the GC sees the values as rooted
    
    #[inline(always)]
    fn len(&self) -> usize {
        self.stack.len()
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
        self.stack.splice(discard_range, std::iter::empty());
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
        self.stack.splice(replace_range, std::iter::once(value));
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
            Closure::Open(index) => self.peek_at(*index).clone(),
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
    upvalues: HashMap<usize, Vec<UpvalueRef>>,
}

impl OpenUpvalues {
    fn new() -> Self {
        Self {
            upvalues: HashMap::with_hasher(Default::default())
        }
    }
    
    fn insert_ref(&mut self, upval_ref: UpvalueRef) {
        // let weak_ref = upval_ref.into()  // TODO use weak references
        
        let index = match upval_ref.borrow().value() {
            Closure::Open(index) => index,
            _ => panic!("insert non-open upvalue"),
        };
        
        self.upvalues.entry(index)
            .and_modify(|refs| refs.push(upval_ref))
            .or_insert(vec![ upval_ref ]);
    }
    
    fn close_upvalues(&mut self, _value: usize) {
        unimplemented!()
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
    stop: bool,
}

impl<'m> From<VirtualMachine<'m>> for VMStepper<'m> {
    fn from(vm: VirtualMachine<'m>) -> Self {
        Self { vm, stop: false }
    }
}

impl<'m> Iterator for VMStepper<'m> {
    type Item = ExecResult<VMSnapshot>;
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.stop {
            return None
        }
        
        let status = self.vm.exec_next();
        if let Err(error) = status {
            self.stop = true;
            return Some(Err(error));
        }
        
        self.stop = status.unwrap();
        Some(Ok(VMSnapshot::from(&self.vm)))
    }
}