use crate::language::{IntType, FloatType};
use crate::codegen::{Program, ProgramData, Chunk, ChunkID, ConstID, Constant, OpCode};
use crate::runtime::Variant;
use crate::runtime::gc::GC;
use crate::runtime::ops;
use crate::runtime::function::Call;
use crate::runtime::strings::StringSymbol;
use crate::runtime::module::{Module, Access, GlobalEnv};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};
use crate::debug::traceback::TraceSite;
use crate::debug::snapshot::{VMSnapshot, VMStateSnapshot};


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


// Stack-based Virtual Machine
#[derive(Debug)]
pub struct VirtualMachine<'c> {
    traceback: Vec<TraceSite>,
    
    calls: Vec<VMState<'c>>,
    values: ValueStack,
    state: VMState<'c>,
}

impl<'c> VirtualMachine<'c> {
    /// Create a new VM with the specified root module and an empty main chunk
    pub fn new(main_module: GC<Module>, main_chunk: &'c [u8]) -> Self {
        Self {
            traceback: Vec::new(),
            calls: Vec::new(),
            values: ValueStack::new(),
            state: VMState::main_chunk(main_module, main_chunk),
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
        let control = self.state.exec_next(&mut self.values)
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
                
                let mut state = VMState::call_frame(module, chunk_id, call.frame, locals);
                std::mem::swap(&mut self.state, &mut state);
                self.calls.push(state);
                
                log::debug!("Setup call: {{ frame: {}, locals: {} }}", self.state.frame, self.state.locals);
                log::debug!("Stack: {:?}", self.values);
            },
        }
        
        Ok(())
    }
    
    fn return_call(&mut self) {
        let frame = self.state.frame;
        
        let mut state = self.calls.pop().expect("empty call stack");
        std::mem::swap(&mut self.state, &mut state);
        
        let retval = self.values.pop();
        self.values.truncate(frame.into());
        self.values.push(retval);
        self.traceback.pop();
        
        log::debug!("Return call: {{ frame: {}, locals: {} }}", self.state.frame, self.state.locals);
        log::debug!("Stack: {:?}", self.values);
    }
}

// Helper macros
macro_rules! read_le_bytes {
    ( $type:ty, $data:expr ) => {
        <$type>::from_le_bytes($data.try_into().unwrap())
    };
}

macro_rules! eval_unary_op {
    ( $stack:expr, $eval_func:tt ) => {
        {
            let result = ops::$eval_func($stack.peek())?;
            $stack.replace(result);
        }
    }
}

macro_rules! eval_binary_op {
    ( $stack:expr, $eval_func:tt ) => {
        {
            // need to pop only after eval, to ensure operands stay rooted in GC
            let operand = $stack.peek_many(2);
            let result = ops::$eval_func(&operand[0], &operand[1])?;
            $stack.pop();
            $stack.replace(result);
        }
    };
}

macro_rules! eval_cmp {
    ( $stack:expr, $eval_func:tt ) => {
        {
            let operand = $stack.peek_many(2);
            let result = ops::$eval_func(&operand[0], &operand[1])?;
            $stack.pop();
            $stack.replace(Variant::from(result));
        }
    };
}

macro_rules! cond_jump {
    ( $state:expr, $cond:expr, $offset:expr ) => {
        {
            let mut offset = $offset;
            offset &= isize::from(!$cond).wrapping_sub(1);
            $state.pc = $state.offset_pc(offset).expect("pc overflow/underflow");
        }
    }
}


pub type LocalIndex = u16;

#[derive(Debug)]
struct VMState<'c> {
    module: GC<Module>,
    chunk: &'c [u8],
    chunk_id: Chunk,
    frame: usize,        // start index for locals belonging to this frame
    locals: LocalIndex,  // local variable count
    pc: usize,
}

impl<'c> VMState<'c> {
    fn call_frame(module: GC<Module>, chunk_id: ChunkID, frame: usize, locals: LocalIndex) -> Self {
        
        // This hack allows us to get around the self-referentiality of storing both "module" and "chunk"
        // in the same struct. The alternative would be to store "chunk" as an `Option<Box<[u8]>>` and call 
        // `module.data().get_chunk()` before every single instruction, but I want to avoid the overhead of that.
        // SAFETY: This is safe because "module" is rooted as long as this VMState is in the call stack, 
        // and VMStates are never stored outside of a VirtualMachine's call stack.
        let chunk: *const [u8] = module.data().get_chunk(chunk_id);
        let chunk = unsafe { chunk.as_ref::<'c>().unwrap() };
        
        Self {
            module,
            chunk,
            chunk_id: Chunk::ChunkID(chunk_id),
            frame,
            locals,
            pc: 0,
        }
    }
    
    fn main_chunk(module: GC<Module>, chunk: &'c [u8]) -> Self {
        Self {
            module,
            chunk,
            chunk_id: Chunk::Main,
            frame: 0,
            locals: 0,
            pc: 0,
        }
    }

    #[inline(always)]
    fn offset_pc(&self, offset: isize) -> Option<usize> {
        if offset >= 0 {
            usize::checked_add(self.pc, offset as usize)
        } else {
            usize::checked_sub(self.pc, offset.unsigned_abs())
        }
    }
    
    #[inline]
    fn into_name(value: Variant) -> StringSymbol {
        match value {
            Variant::String(symbol) => symbol,
            _ => panic!("invalid operand"),
        }
    }
    
    #[inline]
    fn into_usize(value: Variant) -> usize {
        if let Variant::Integer(value) = value {
            if let Ok(value) = usize::try_from(value) {
                return value;
            }
        }
        panic!("invalid operand")
    }

    // convert a local variable index to an index into the value stack
    #[inline]
    fn local_index(&self, local: LocalIndex) -> usize {
        self.frame + usize::from(local)
    }
    
    fn get_trace(&self, offset: usize) -> TraceSite {
        TraceSite::Chunk {
            offset,
            module: self.module,
            chunk_id: self.chunk_id,
        }
    }

    #[inline(always)]
    fn exec_next(&mut self, stack: &mut ValueStack) -> ExecResult<Control> {
        let op_byte = self.chunk.get(self.pc).expect("pc out of bounds");
        let opcode = OpCode::from_byte(*op_byte)
            .unwrap_or_else(|| panic!("invalid instruction: {:x}", op_byte));
        
        let data_slice = (self.pc + 1) .. (self.pc + opcode.instr_len());
        let current_offset = self.pc;
        self.pc += opcode.instr_len(); // pc points to next instruction
        
        let data = self.chunk.get(data_slice).expect("truncated instruction");
        
        self.exec_instruction(current_offset, opcode, data, stack)
            .map_err(|error| error.push_frame(self.get_trace(current_offset)))
    }
    
    #[inline(always)]
    fn exec_instruction(&mut self, current_offset: usize, opcode: OpCode, data: &[u8], stack: &mut ValueStack) -> ExecResult<Control> {
        match opcode {
            OpCode::Nop => { },
            
            OpCode::Return => return Ok(Control::Return),
            
            OpCode::Call => {
                const SYSTEM_ARGS: usize = 2; // [ callee, nargs, ... ]
                
                let nargs = Self::into_usize(stack.peek().clone());
                let call_locals = SYSTEM_ARGS + nargs;
                
                stack.swap_last(stack.len() - call_locals + 1);
                
                let frame = stack.len() - call_locals;
                let callee = stack.peek_at(frame);
                
                let args = stack.peek_many(nargs);
                let call = CallInfo {
                    nargs,
                    frame,
                    call: callee.invoke(args)?,
                    site: self.get_trace(current_offset),
                };
                return Ok(Control::Call(call))
            },
            
            OpCode::CallUnpack => unimplemented!(),
            
            OpCode::Pop => { 
                stack.pop(); 
            },
            OpCode::Drop => { 
                let count = usize::from(data[0]);
                stack.discard(count); 
            }
            OpCode::Clone => {
                stack.push(stack.peek().clone());
            }
            
            OpCode::LoadConst => {
                let cid = ConstID::from(data[0]);
                let value = Module::get_const(self.module, cid);
                stack.push(value);
            },
            OpCode::LoadConst16 => {
                let cid = ConstID::from(read_le_bytes!(u16, data));
                let value = Module::get_const(self.module, cid);
                stack.push(value);
            },
            
            OpCode::InsertGlobal => {
                let name = Self::into_name(stack.pop());
                let value = stack.peek().clone();
                self.module.globals().borrow_mut().create(name, Access::ReadOnly, value);
            },
            OpCode::InsertGlobalMut => {
                let name = Self::into_name(stack.pop());
                let value = stack.peek().clone();
                self.module.globals().borrow_mut().create(name, Access::ReadWrite, value);
            },
            OpCode::StoreGlobal => {
                let name = Self::into_name(stack.pop());
                let value = stack.peek().clone();
                
                let mut globals = self.module.globals().borrow_mut();
                let store = globals.lookup_mut(&name)?;
                *store = value;
            },
            OpCode::LoadGlobal => {
                let value = {
                    let name = Self::into_name(stack.peek().clone());
                    self.module.globals().borrow().lookup(&name)?.clone()
                };
                stack.replace(value);
            },
            
            OpCode::InsertLocal => {
                let value = stack.peek().clone();
                stack.insert(self.local_index(self.locals), value);
                self.locals += 1;
            },
            OpCode::StoreLocal => {
                let index = LocalIndex::from(data[0]);
                stack.replace_at(self.local_index(index), stack.peek().clone());
            },
            OpCode::StoreLocal16 => {
                let index = LocalIndex::from(read_le_bytes!(u16, data));
                stack.replace_at(self.local_index(index), stack.peek().clone());
            },
            OpCode::LoadLocal => {
                let index = LocalIndex::from(data[0]);
                debug_assert!(index < self.locals);
                stack.push(stack.peek_at(self.local_index(index)).clone());
            },
            OpCode::LoadLocal16 => {
                let index = LocalIndex::from(read_le_bytes!(u16, data));
                debug_assert!(index < self.locals.into());
                stack.push(stack.peek_at(self.local_index(index)).clone());
            },
            OpCode::DropLocals => {
                let count = LocalIndex::from(data[0]);
                debug_assert!(count <= self.locals);
                
                let locals_end_index = self.local_index(self.locals);
                if stack.len() == locals_end_index {
                    stack.discard(usize::from(count));
                } else {
                    let index = locals_end_index - usize::from(count);
                    stack.discard_at(index, usize::from(count));
                }
                self.locals -= count;
            },
            
            OpCode::InsertUpvalueLocal => {
                unimplemented!()
            }
            OpCode::InsertUpvalueLocal16 => {
                unimplemented!()
            }
            OpCode::InsertUpvalueExtern => {
                unimplemented!()
            }
            OpCode::InsertUpvalueExtern16 => {
                unimplemented!()
            }
            OpCode::LoadUpvalue => {
                unimplemented!()
            }
            OpCode::LoadUpvalue16 => {
                unimplemented!()
            }
            
            OpCode::Nil => stack.push(Variant::Nil),
            OpCode::True => stack.push(Variant::BoolTrue),
            OpCode::False => stack.push(Variant::BoolFalse),
            OpCode::Empty => stack.push(Variant::EmptyTuple),
            
            OpCode::Tuple => {
                let tuple_len = usize::from(data[0]);
                
                let items = stack.pop_many(tuple_len).into_boxed_slice();
                stack.push(Variant::from(items));
            },
            OpCode::TupleN => {
                let tuple_len = Self::into_usize(stack.pop());
                
                let items = stack.pop_many(tuple_len).into_boxed_slice();
                stack.push(Variant::from(items));
            },
            
            OpCode::UInt8 => {
                let value = IntType::from(data[0]);
                stack.push(Variant::Integer(value))
            },
            OpCode::Int8 => {
                let value = i8::from_le_bytes([data[0]]);
                stack.push(Variant::Integer(IntType::from(value)))
            }
            OpCode::Float8 => {
                let value = i8::from_le_bytes([data[0]]);
                stack.push(Variant::Float(FloatType::from(value)))
            }
            
            OpCode::Neg => eval_unary_op!(stack, eval_neg),
            OpCode::Pos => eval_unary_op!(stack, eval_pos),
            OpCode::Inv => eval_unary_op!(stack, eval_inv),
            OpCode::Not => eval_unary_op!(stack, eval_not),
            
            OpCode::And => eval_binary_op!(stack, eval_and),
            OpCode::Xor => eval_binary_op!(stack, eval_xor),
            OpCode::Or  => eval_binary_op!(stack, eval_or),
            OpCode::Shl => eval_binary_op!(stack, eval_shl),
            OpCode::Shr => eval_binary_op!(stack, eval_shr),
            OpCode::Add => eval_binary_op!(stack, eval_add),
            OpCode::Sub => eval_binary_op!(stack, eval_sub),
            OpCode::Mul => eval_binary_op!(stack, eval_mul),
            OpCode::Div => eval_binary_op!(stack, eval_div),
            OpCode::Mod => eval_binary_op!(stack, eval_mod),
            
            OpCode::EQ => eval_cmp!(stack, eval_eq),
            OpCode::NE => eval_cmp!(stack, eval_ne),
            OpCode::LT => eval_cmp!(stack, eval_lt),
            OpCode::LE => eval_cmp!(stack, eval_le),
            OpCode::GE => eval_cmp!(stack, eval_ge),
            OpCode::GT => eval_cmp!(stack, eval_gt),
            
            OpCode::Jump => {
                let offset = isize::from(read_le_bytes!(i16, data));
                self.pc = self.offset_pc(offset).expect("pc overflow/underflow");
            }
            OpCode::LongJump => {
                let offset = isize::try_from(read_le_bytes!(i32, data)).unwrap();
                self.pc = self.offset_pc(offset).expect("pc overflow/underflow");
            }
            
            OpCode::JumpIfFalse    => cond_jump!(self, !stack.peek().truth_value(), isize::from(read_le_bytes!(i16, data))),
            OpCode::JumpIfTrue     => cond_jump!(self, stack.peek().truth_value(),  isize::from(read_le_bytes!(i16, data))),
            OpCode::PopJumpIfFalse => cond_jump!(self, !stack.pop().truth_value(),  isize::from(read_le_bytes!(i16, data))),
            OpCode::PopJumpIfTrue  => cond_jump!(self, stack.pop().truth_value(),   isize::from(read_le_bytes!(i16, data))),
            
            OpCode::LongJumpIfFalse    => cond_jump!(self, !stack.peek().truth_value(), isize::try_from(read_le_bytes!(i32, data)).unwrap()),
            OpCode::LongJumpIfTrue     => cond_jump!(self, stack.peek().truth_value(),  isize::try_from(read_le_bytes!(i32, data)).unwrap()),
            OpCode::PopLongJumpIfFalse => cond_jump!(self, !stack.pop().truth_value(),  isize::try_from(read_le_bytes!(i32, data)).unwrap()),
            OpCode::PopLongJumpIfTrue  => cond_jump!(self, stack.pop().truth_value(),   isize::try_from(read_le_bytes!(i32, data)).unwrap()),
            
            OpCode::Inspect => println!("{:?}", stack.peek()),
            OpCode::Assert => {
                if !stack.peek().truth_value() {
                    return Err(ErrorKind::AssertFailed.into());
                }
            }
        }
        
        Ok(Control::Continue)
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
    
}


// For debugging



impl From<&VirtualMachine<'_>> for VMSnapshot {
    fn from (vm: &VirtualMachine) -> Self {
        Self {
            calls: vm.calls.iter().map(VMStateSnapshot::from).collect(),
            values: vm.values.stack.clone(),
            state: (&vm.state).into(),
        }
    }
}



impl From<&VMState<'_>> for VMStateSnapshot {
    fn from(state: &VMState) -> Self {
        let next_instr = state.chunk.get(state.pc)
            .map(|byte| OpCode::try_from(*byte).map_or_else(
                |byte| vec![ byte ],
                |opcode| state.chunk[state.pc..(state.pc + opcode.instr_len())].to_vec()
            ));
        
        Self {
            module: state.module.to_string(),
            chunk_id: state.chunk_id,
            frame: state.frame,
            locals: state.locals,
            pc: state.pc,
            next_instr,
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