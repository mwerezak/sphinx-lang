use std::fmt;

// Opcodes

// Rust enums are not like C enums! They're more like unions.
// So if we want to convert between them and integer constants easily, 
// we need to explictly define each value as a const

const OP_CONST:  u8 = 0x1;  // load a constant from the chunk's const pool
const OP_RETURN: u8 = 0xF;  // return from current function


#[repr(u8)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum OpCode {
    Const  = OP_CONST,
    Return = OP_RETURN, 
}

impl OpCode {
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        let opcode = match byte {
            OP_CONST => Self::Const,
            OP_RETURN => Self::Return,
            _ => return None,
        };
        Some(opcode)
    }
}

impl From<OpCode> for u8 {
    fn from(opcode: OpCode) -> Self {
        match opcode {
            OpCode::Const => OP_CONST,
            OpCode::Return => OP_RETURN,
        }
    }
}

impl PartialEq<u8> for OpCode {
    fn eq(&self, other: &u8) -> bool {
        *other == (*self).into()
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match *self {
            Self::Const => "OP_CONST",
            Self::Return => "OP_RETURN",
        };
        fmt.write_str(mnemonic)
    }
}

// Chunks

pub struct Chunk {
    bytes: Vec<u8>,
    // consts: 
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            bytes: Vec::new(),
        }
    }
    
    pub fn bytes(&self) -> &[u8] {
        self.bytes.as_slice()
    }
    
    pub fn push<C: Into<u8>>(&mut self, byte: C) {
        self.bytes.push(byte.into());
    }
    
    // pub fn add_const(&mut self, const: ?) -> ConstIdx;
    // pub fn get_const(&self, index: ConstIdx) -> ?;
}