use crate::codegen::{Chunk, ConstID, OpCode};
use crate::runtime::Variant;
use crate::runtime::ops;
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

// Stack-based Virtual Machine
#[derive(Debug)]
pub struct VirtualMachine {
    pc: usize, // program counter
    program: Chunk,
    globals: Namespace,
    immediate: Vec<Variant>,
}

impl VirtualMachine {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            pc: 0,
            program: chunk,
            globals: Namespace::new(),
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
    fn pop_stack(&mut self) -> Variant {
        self.immediate.pop().expect("empty stack")
    }
    
    #[inline(always)]
    fn pop_many(&mut self, count: usize) -> Vec<Variant> {
        self.immediate.split_off(self.immediate.len() - count)
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
    fn peek_many(&self, count: usize) -> &[Variant] {
        let (_, peek) = self.immediate.as_slice().split_at(self.immediate.len()-count);
        peek
    }
    
    fn exec_instr(&mut self, opcode: OpCode) -> ExecResult<Control> {
        let len = opcode.instr_len();
        let data_slice = (self.pc+1)..(self.pc+len);
        let data = self.program.bytes().get(data_slice).expect("truncated instruction");
        
        self.pc += opcode.instr_len(); // pc points to next instruction
        
        match opcode {
            OpCode::Return => return Ok(Control::Return),
            
            OpCode::Pop => { self.pop_stack(); },
            
            OpCode::LoadConst => {
                let value = self.program.lookup_value(data[0]);
                self.push_stack(value);
            },
            
            OpCode::LoadConst16 => {
                let cid = ConstID::from_le_bytes([data[0], data[1]]);
                let value = self.program.lookup_value(cid);
                self.push_stack(value);
            },
            
            OpCode::InsertGlobal => unimplemented!(),
            OpCode::InsertGlobal16 => unimplemented!(),
            OpCode::InsertGlobalMut => unimplemented!(),
            OpCode::InsertGlobalMut16 => unimplemented!(),
            
            OpCode::Nil => self.push_stack(Variant::Nil),
            OpCode::Empty => self.push_stack(Variant::EmptyTuple),
            OpCode::True => self.push_stack(Variant::BoolTrue),
            OpCode::False => self.push_stack(Variant::BoolFalse),
            
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
            OpCode::Or => eval_binary_op!(self, eval_or),
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
            
            OpCode::Inspect => println!("{:?}", self.pop_stack()),
            OpCode::Dump => println!("DBG_DUMP: {:?}", self),
        }
        
        Ok(Control::None)
    }
}
