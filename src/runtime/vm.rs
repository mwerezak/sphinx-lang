use crate::codegen::{Chunk, ConstID, OpCode};
use crate::runtime::Variant;
use crate::runtime::module::{Access, Global};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


// Helper
enum Control {
    None,
    Return,
}

// Stack-based Virtual Machine
pub struct VirtualMachine<'c> {
    pc: usize, // program counter
    program: &'c Chunk,
    globals: Vec<Global>,
    immediate: Vec<Variant>,
}

impl VirtualMachine<'_> {
    pub fn run(&mut self) -> ExecResult<()> {
        let program_text = self.program.bytes();
        
        loop {
            
            let op_byte = program_text[self.pc];
            let opcode = OpCode::from_byte(op_byte)
                .unwrap_or_else(|| panic!("invalid instruction: {:x}", op_byte));
            
            let len = opcode.instr_len();
            let data = &program_text[(self.pc+1)..(self.pc+len)];
            self.pc += len;
            
            match self.exec_instr(opcode, data)? {
                Control::None => { }
                Control::Return => return Ok(())
            }
        }
    }
    
    // Note: when moving values off the stack, make sure to copy *before* popping
    // This ensures that the GC sees the values as rooted
    
    fn pop_stack(&mut self) -> Variant {
        self.immediate.pop().expect("empty stack")
    }
    
    fn pop_many(&mut self, count: usize) -> Vec<Variant> {
        self.immediate.split_off(self.immediate.len() - count)
    }
    
    fn push_stack(&mut self, value: Variant) {
        self.immediate.push(value)
    }
    
    fn replace_stack(&mut self, value: Variant) {
        *self.immediate.last_mut().expect("empty stack") = value;
    }
    
    fn peek_stack(&self) -> &Variant {
        self.immediate.last().expect("empty stack")
    }
    
    fn peek_many(&self, count: usize) -> &[Variant] {
        let (_, peek) = self.immediate.as_slice().split_at(self.immediate.len()-count);
        peek
    }
    
    fn exec_instr(&mut self, opcode: OpCode, data: &[u8]) -> ExecResult<Control> {
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
            
            OpCode::Nil => self.push_stack(Variant::Nil),
            OpCode::Empty => self.push_stack(Variant::EmptyTuple),
            OpCode::True => self.push_stack(Variant::BoolTrue),
            OpCode::False => self.push_stack(Variant::BoolFalse),
            
            OpCode::Neg => unimplemented!(),
            OpCode::Pos => unimplemented!(),
            OpCode::Inv => unimplemented!(),
            OpCode::Not => unimplemented!(),
            
            OpCode::And => unimplemented!(),
            OpCode::Xor => unimplemented!(),
            OpCode::Or => unimplemented!(),
            OpCode::Shl => unimplemented!(),
            OpCode::Shr => unimplemented!(),
            OpCode::Add => unimplemented!(),
            OpCode::Sub => unimplemented!(),
            OpCode::Mul => unimplemented!(),
            OpCode::Div => unimplemented!(),
            OpCode::Mod => unimplemented!(),
            OpCode::EQ => unimplemented!(),
            OpCode::NE => unimplemented!(),
            OpCode::LT => unimplemented!(),
            OpCode::LE => unimplemented!(),
            OpCode::GE => unimplemented!(),
            OpCode::GT => unimplemented!(),
        }
        
        Ok(Control::None)
    }
}


