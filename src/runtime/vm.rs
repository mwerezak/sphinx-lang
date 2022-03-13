use crate::codegen::{Chunk, OpCode};
use crate::runtime::Variant;
use crate::runtime::errors::RuntimeError;

// Stack-based Virtual Machine
struct VirtualMachine<'c> {
    pc: usize, // program counter
    program: &'c Chunk,
    //globals: HashMap<StringSymbol, Variable>
    immediate: Vec<Variant>,
}

impl VirtualMachine<'_> {
    
    pub fn exec(&mut self) -> Result<(), RuntimeError> {
        loop {
            
            let byte = self.program.bytes()[self.pc];
            self.pc += 1;
            let instr = OpCode::from_byte(byte)
                .unwrap_or_else(|| panic!("invalid instruction: {:x}", byte));
            
            match instr {
                OpCode::Return => {
                    return Ok(());
                },
                
                OpCode::LoadConst => {
                    
                },
                
                OpCode::LoadConst16 => {
                    
                },
                
                OpCode::Nil => self.immediate.push(Variant::Nil),
                OpCode::Empty => self.immediate.push(Variant::EmptyTuple),
                OpCode::True => self.immediate.push(Variant::BoolTrue),
                OpCode::False => self.immediate.push(Variant::BoolFalse),
                
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
        }
    }
}


