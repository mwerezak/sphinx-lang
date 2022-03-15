use crate::codegen::{Chunk, ConstID, OpCode};
use crate::runtime::Variant;
use crate::runtime::module::{Access, Namespace};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


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
    
    // Supports execution in REPL mode. Swaps out the the current program while leaving globals untouched.
    // If the immediate stack is not empty then this will panic
    pub fn reload_program(&mut self, chunk: Chunk) -> () {
        if !self.immediate.is_empty() {
            panic!("invalid state for program reload");
        }
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
            
            OpCode::Inspect => println!("{}", self.pop_stack()),
            OpCode::Dump => println!("DBG_DUMP: {:?}", self),
        }
        
        Ok(Control::None)
    }
}
