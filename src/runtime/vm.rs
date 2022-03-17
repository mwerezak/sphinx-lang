use crate::language::{IntType, FloatType};
use crate::codegen::{Chunk, ConstID, OpCode};
use crate::runtime::Variant;
use crate::runtime::ops;
use crate::runtime::strings::StringSymbol;
use crate::runtime::module::{Access, Namespace};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


// Helper macros
macro_rules! eval_binary_op {
    ($self:expr, $eval_func:tt ) => {
        {
            // need to pop only after eval, to ensure operands stay rooted in GC
            let operand = $self.peek_many(2);
            let result = ops::$eval_func(&operand[0], &operand[1])?;
            $self.pop_stack();
            $self.replace_stack(result);
        }
    };
}

macro_rules! eval_cmp {
    ($self:expr, $eval_func:tt ) => {
        {
            let operand = $self.peek_many(2);
            let result = ops::$eval_func(&operand[0], &operand[1])?;
            $self.pop_stack();
            $self.replace_stack(Variant::from(result));
        }
    };
}


// Helper
enum Control {
    None,
    Return,
}

pub type LocalIndex = u16;

// Stack-based Virtual Machine
#[derive(Debug)]
pub struct VirtualMachine {
    pc: usize, // program counter
    program: Chunk,
    globals: Namespace,
    locals: LocalIndex,
    immediate: Vec<Variant>,
}

impl VirtualMachine {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            pc: 0,
            program: chunk,
            globals: Namespace::new(),
            locals: 0,
            immediate: Vec::new(),
        }
    }
    
    pub fn reload_program(&mut self, chunk: Chunk) {
        self.immediate.clear();
        self.program = chunk;
        self.pc = 0;
    }
    
    pub fn take_chunk(self) -> Chunk { self.program }
    
    pub fn run(&mut self) -> ExecResult<()> {
        loop {
            let op_byte = self.program.bytes().get(self.pc).expect("pc out of bounds");
            
            let opcode = OpCode::from_byte(*op_byte)
                .unwrap_or_else(|| panic!("invalid instruction: {:x}", op_byte));
            
            match self.exec_instr(opcode)? {
                Control::None => { }
                Control::Return => return Ok(())
            }
        }
    }
    
    // Note: when moving values off the stack, make sure to copy *before* popping
    // This ensures that the GC sees the values as rooted
    
    #[inline(always)]
    fn stack_len(&self) -> usize {
        self.immediate.len()
    }
    
    #[inline(always)]
    fn pop_stack(&mut self) -> Variant {
        self.immediate.pop().expect("empty stack")
    }
    
    #[inline(always)]
    fn discard_stack(&mut self, count: usize) {
        self.immediate.truncate(self.immediate.len() - count)
    }
    
    #[inline(always)]
    fn push_stack(&mut self, value: Variant) {
        self.immediate.push(value)
    }
    
    #[inline(always)]
    fn replace_stack(&mut self, value: Variant) {
        *self.immediate.last_mut().expect("empty stack") = value;
    }
    
    #[inline(always)]
    fn peek_stack(&self) -> &Variant {
        self.immediate.last().expect("empty stack")
    }
    
    #[inline(always)]
    fn peek_offset(&self, offset: usize) -> &Variant {
        self.immediate.get(offset)
            .expect("stack offset out of bounds")
    }
    
    #[inline(always)]
    fn replace_offset(&mut self, offset: usize, value: Variant) {
        let item = self.immediate.get_mut(offset)
            .expect("stack offset out of bounds");
        
        *item = value;
    }
    
    #[inline(always)]
    fn peek_many(&self, count: usize) -> &[Variant] {
        let (_, peek) = self.immediate.as_slice().split_at(self.immediate.len()-count);
        peek
    }
    
    #[inline(always)]
    fn offset_pc(&self, offset: isize) -> Option<usize> {
        if offset >= 0 {
            usize::checked_add(self.pc, offset as usize)
        } else {
            usize::checked_sub(self.pc, offset.unsigned_abs())
        }
    }
    
    
    fn into_name(value: &Variant) -> StringSymbol {
        match value {
            Variant::String(symbol) => *symbol,
            _ => panic!("invalid operand type"),
        }
    }
    
    fn exec_instr(&mut self, opcode: OpCode) -> ExecResult<Control> {
        let len = opcode.instr_len();
        let data_slice = (self.pc+1)..(self.pc+len);
        let data = self.program.bytes().get(data_slice).expect("truncated instruction");
        
        self.pc += opcode.instr_len(); // pc points to next instruction
        
        match opcode {
            OpCode::Nop => { },
            
            OpCode::Return => return Ok(Control::Return),
            
            OpCode::Pop => { 
                self.pop_stack(); 
            },
            OpCode::Drop => { 
                let count = data[0].into();
                self.discard_stack(count); 
            }
            OpCode::Clone => {
                self.push_stack(self.peek_stack().clone());
            }
            
            OpCode::LoadConst => {
                let value = self.program.lookup_value(data[0]);
                self.push_stack(value);
            },
            OpCode::LoadConst16 => {
                let cid = ConstID::from_le_bytes([data[0], data[1]]);
                let value = self.program.lookup_value(cid);
                self.push_stack(value);
            },
            
            OpCode::InsertGlobal => {
                let name = Self::into_name(&self.pop_stack());
                let value = self.peek_stack().clone();
                self.globals.create(name, Access::ReadOnly, value)?;
            },
            OpCode::InsertGlobalMut => {
                let name = Self::into_name(&self.pop_stack());
                let value = self.peek_stack().clone();
                self.globals.create(name, Access::ReadWrite, value)?;
            },
            OpCode::StoreGlobal => {
                let name = Self::into_name(&self.pop_stack());
                let value = self.peek_stack().clone();
                let store = self.globals.lookup_mut(&name)?;
                *store = value;
            },
            OpCode::LoadGlobal => {
                let value = {
                    let name = Self::into_name(self.peek_stack());
                    self.globals.lookup(&name)?.clone()
                };
                self.replace_stack(value);
            },
            
            OpCode::InsertLocal => {
                let value = self.peek_stack().clone();
                self.immediate.insert(self.locals.into(), value);
                self.locals += 1;
            },
            OpCode::StoreLocal => {
                let offset = usize::from(data[0]);
                self.replace_offset(offset, self.peek_stack().clone());
            },
            OpCode::StoreLocal16 => {
                let offset = u16::from_le_bytes([data[0], data[1]]);
                let offset = usize::from(offset);
                self.replace_offset(offset, self.peek_stack().clone());
            },
            OpCode::LoadLocal => {
                let offset = usize::from(data[0]);
                self.push_stack(self.peek_offset(offset).clone());
            },
            OpCode::LoadLocal16 => {
                let offset = u16::from_le_bytes([data[0], data[1]]);
                let offset = usize::from(offset);
                self.push_stack(self.peek_offset(offset).clone());
            },
            OpCode::DropLocals => {
                debug_assert!(usize::from(self.locals) == self.stack_len(), "drop with immediate values on stack");
                let count = data[0];
                self.discard_stack(count.into());
                self.locals -= LocalIndex::from(count);
            },
            
            OpCode::Nil => self.push_stack(Variant::Nil),
            OpCode::True => self.push_stack(Variant::BoolTrue),
            OpCode::False => self.push_stack(Variant::BoolFalse),
            OpCode::Empty => self.push_stack(Variant::EmptyTuple),

            OpCode::Tuple => {
                let tuple_len = usize::from(data[0]);
                let items = self.immediate.split_off(self.stack_len() - tuple_len).into_boxed_slice();
                self.push_stack(Variant::make_tuple(items));
            },
            OpCode::UInt8 => {
                let value = IntType::from(data[0]);
                self.push_stack(Variant::Integer(value))
            },
            OpCode::Int8 => {
                let value = i8::from_le_bytes([data[0]]);
                self.push_stack(Variant::Integer(IntType::from(value)))
            }
            OpCode::Float8 => {
                let value = i8::from_le_bytes([data[0]]);
                self.push_stack(Variant::Float(FloatType::from(value)))
            }
            
            OpCode::Neg => {
                let result = ops::eval_neg(self.peek_stack())?;
                self.replace_stack(result);
            },
            OpCode::Pos => {
                let result = ops::eval_pos(self.peek_stack())?;
                self.replace_stack(result);
            },
            OpCode::Inv => {
                let result = ops::eval_inv(self.peek_stack())?;
                self.replace_stack(result);
            },
            OpCode::Not => {
                let result = ops::eval_not(self.peek_stack())?;
                self.replace_stack(result);
            },
            
            OpCode::And => eval_binary_op!(self, eval_and),
            OpCode::Xor => eval_binary_op!(self, eval_xor),
            OpCode::Or  => eval_binary_op!(self, eval_or),
            OpCode::Shl => eval_binary_op!(self, eval_shl),
            OpCode::Shr => eval_binary_op!(self, eval_shr),
            OpCode::Add => eval_binary_op!(self, eval_add),
            OpCode::Sub => eval_binary_op!(self, eval_sub),
            OpCode::Mul => eval_binary_op!(self, eval_mul),
            OpCode::Div => eval_binary_op!(self, eval_div),
            OpCode::Mod => eval_binary_op!(self, eval_mod),
            
            OpCode::EQ => eval_cmp!(self, eval_eq),
            OpCode::NE => eval_cmp!(self, eval_ne),
            OpCode::LT => eval_cmp!(self, eval_lt),
            OpCode::LE => eval_cmp!(self, eval_le),
            OpCode::GE => eval_cmp!(self, eval_ge),
            OpCode::GT => eval_cmp!(self, eval_gt),
            
            OpCode::Jump => {
                let offset = i16::from_le_bytes([data[0], data[1]]);
                self.pc = self.offset_pc(offset.into()).expect("pc overflow/underflow");
            }
            OpCode::JumpIfFalse => {
                let cond = self.peek_stack().truth_value();
                let offset = i16::from(cond).wrapping_sub(1) & i16::from_le_bytes([data[0], data[1]]);
                self.pc = self.offset_pc(offset.into()).expect("pc overflow/underflow");
            }
            OpCode::JumpIfTrue => {
                let cond = self.peek_stack().truth_value();
                let offset = i16::from(!cond).wrapping_sub(1) & i16::from_le_bytes([data[0], data[1]]);
                self.pc = self.offset_pc(offset.into()).expect("pc overflow/underflow");
            }
            OpCode::PopJumpIfFalse => {
                let mut offset = i16::from_le_bytes([data[0], data[1]]);
                let cond = self.pop_stack().truth_value();
                offset &= i16::from(cond).wrapping_sub(1);
                self.pc = self.offset_pc(offset.into()).expect("pc overflow/underflow");
            }
            OpCode::PopJumpIfTrue => {
                let mut offset = i16::from_le_bytes([data[0], data[1]]);
                let cond = self.pop_stack().truth_value();
                offset &= i16::from(!cond).wrapping_sub(1);
                self.pc = self.offset_pc(offset.into()).expect("pc overflow/underflow");
            }
            
            
            OpCode::Inspect => println!("{:?}", self.pop_stack()),
            OpCode::Assert => {
                if !self.pop_stack().truth_value() {
                    return Err(ErrorKind::AssertFailed.into());
                }
            }
        }
        
        Ok(Control::None)
    }
}
