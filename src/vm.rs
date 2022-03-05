pub mod chunk;
pub mod opcodes;


use chunk::Chunk;
use opcodes::OpCode;

use crate::runtime::errors::RuntimeError;


// Stack-based Virtual Machine
struct VirtualMachine<'c> {
    pc: usize, // program counter
    program: &'c Chunk,
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
                
                OpCode::LoadConstWide => {
                    
                },
            }
        }
    }
}


