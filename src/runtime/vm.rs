use crate::language::{IntType, FloatType};
use crate::codegen::{Program, ProgramData, ChunkID, ConstID, Constant, OpCode};
use crate::runtime::Variant;
use crate::runtime::ops;
use crate::runtime::strings::StringSymbol;
use crate::runtime::module::{Module, ModuleCache, ModuleID, Access, GlobalEnv};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


enum Control {
    Continue,
    Return,
    Exit,
}

// Stack-based Virtual Machine
#[derive(Debug)]
pub struct VirtualMachine<'m> {
    module_cache: &'m ModuleCache,
    repl_env: Option<&'m GlobalEnv>,
    
    states: Vec<VMState<'m>>,
    values: ValueStack,
}

impl<'m> VirtualMachine<'m> {
    /// Create a new VM with the specified root module and an empty main chunk
    pub fn new(module_cache: &'m ModuleCache, module_id: ModuleID, main_chunk: &'m [u8]) -> Self {
        let module = module_cache.get(&module_id).expect("invalid module id");
        
        Self {
            module_cache,
            repl_env: None,
            values: ValueStack::new(),
            states: vec![ VMState::new(module, main_chunk, 0) ],
        }
    }
    
    pub fn new_repl(module_cache: &'m ModuleCache, repl_env: &'m GlobalEnv, module_id: ModuleID, main_chunk: &'m [u8]) -> Self {
        let module = module_cache.get(&module_id).expect("invalid module id");
        
        Self {
            module_cache,
            repl_env: Some(repl_env),
            values: ValueStack::new(),
            states: vec![ VMState::with_globals(module, repl_env, main_chunk, 0) ],
        }
    }
    
    pub fn run(mut self) -> ExecResult<()> {
        while let Some(state) = self.states.last_mut() {
            match state.exec_next(&mut self.values)? {
                Control::Continue => { }
                Control::Return | Control::Exit => break,
            }
        }
        Ok(())
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
    
    #[inline(always)]
    fn replace_at(&mut self, index: usize, value: Variant) {
        let item = self.stack.get_mut(index)
            .expect("value index out of bounds");
        
        *item = value;
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

// stores the active chunk execution information
#[derive(Debug)]
struct VMState<'m> {
    module: &'m Module,
    globals: &'m GlobalEnv,
    chunk: &'m [u8],
    frame: usize,  // index of the first local
    locals: LocalIndex,
    pc: usize,
}

impl<'m> VMState<'m> {
    fn new(module: &'m Module, chunk: &'m [u8], frame: usize) -> Self {
        Self::with_globals(module, module.globals(), chunk, frame)
    }
    
    fn with_globals(module: &'m Module, globals: &'m GlobalEnv, chunk: &'m [u8], frame: usize) -> Self {
        Self {
            module,
            globals,
            chunk,
            frame,
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

    fn exec_next(&mut self, stack: &mut ValueStack) -> ExecResult<Control> {
        let op_byte = self.chunk.get(self.pc).expect("pc out of bounds");
        let opcode = OpCode::from_byte(*op_byte)
            .unwrap_or_else(|| panic!("invalid instruction: {:x}", op_byte));
        
        let data_slice = (self.pc + 1) .. (self.pc + opcode.instr_len());
        self.pc += opcode.instr_len(); // pc points to next instruction
        
        let data = self.chunk.get(data_slice).expect("truncated instruction");
        match opcode {
            OpCode::Nop => { },
            
            OpCode::Return => return Ok(Control::Return),
            
            OpCode::Call => unimplemented!(),
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
                let value = self.module.get_const(cid);
                stack.push(value);
            },
            OpCode::LoadConst16 => {
                let cid = ConstID::from(read_le_bytes!(u16, data));
                let value = self.module.get_const(cid);
                stack.push(value);
            },
            
            OpCode::InsertGlobal => {
                let name = Self::into_name(stack.pop());
                let value = stack.peek().clone();
                self.globals.borrow_mut().create(name, Access::ReadOnly, value)?;
            },
            OpCode::InsertGlobalMut => {
                let name = Self::into_name(stack.pop());
                let value = stack.peek().clone();
                self.globals.borrow_mut().create(name, Access::ReadWrite, value)?;
            },
            OpCode::StoreGlobal => {
                let name = Self::into_name(stack.pop());
                let value = stack.peek().clone();
                
                let mut globals = self.globals.borrow_mut();
                let store = globals.lookup_mut(&name)?;
                *store = value;
            },
            OpCode::LoadGlobal => {
                let value = {
                    let name = Self::into_name(stack.peek().clone());
                    self.globals.borrow().lookup(&name)?.clone()
                };
                stack.replace(value);
            },
            
            OpCode::InsertLocal => {
                let value = stack.peek().clone();
                stack.insert(self.locals.into(), value);
                self.locals += 1;
            },
            OpCode::StoreLocal => {
                let index = usize::from(data[0]);
                stack.replace_at(index, stack.peek().clone());
            },
            OpCode::StoreLocal16 => {
                let index = usize::from(read_le_bytes!(u16, data));
                stack.replace_at(index, stack.peek().clone());
            },
            OpCode::LoadLocal => {
                let index = usize::from(data[0]);
                debug_assert!(index < self.locals.into());
                stack.push(stack.peek_at(index).clone());
            },
            OpCode::LoadLocal16 => {
                let index = usize::from(read_le_bytes!(u16, data));
                debug_assert!(index < self.locals.into());
                stack.push(stack.peek_at(index).clone());
            },
            OpCode::DropLocals => {
                let count = LocalIndex::from(data[0]);
                if stack.len() == self.locals.into() {
                    stack.discard(usize::from(count));
                } else {
                    let index = usize::from(self.locals) - usize::from(count);
                    stack.discard_at(index, usize::from(count));
                }
                self.locals -= count;
            },
            
            OpCode::Nil => stack.push(Variant::Nil),
            OpCode::True => stack.push(Variant::BoolTrue),
            OpCode::False => stack.push(Variant::BoolFalse),
            OpCode::Empty => stack.push(Variant::EmptyTuple),

            OpCode::Tuple => {
                let tuple_len = usize::from(data[0]);
                let items = stack.pop_many(tuple_len).into_boxed_slice();
                stack.push(Variant::make_tuple(items));
            },
            OpCode::TupleN => {
                let tuple_len = Self::into_usize(stack.pop());
                let items = stack.pop_many(tuple_len).into_boxed_slice();
                stack.push(Variant::make_tuple(items));
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
