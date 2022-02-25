

#[repr(u8)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum OpCode {
    Return = 0,  // return from current function
}

impl From<OpCode> for u8 {
    fn from(opcode: OpCode) -> Self {
        match opcode {
            OpCode::Return => (OpCode::Return as u8),
        }
    }
}

impl PartialEq<u8> for OpCode {
    fn eq(&self, other: &u8) -> bool {
        *other == (*self).into()
    }
}


pub struct Chunk {
    code: Vec<OpCode>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
        }
    }
}