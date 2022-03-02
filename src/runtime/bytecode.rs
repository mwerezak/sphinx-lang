use std::fmt;
use crate::runtime::variant::Variant;

// Opcodes

// Rust enums are not like C enums! They're more like unions.
// So if we want to convert between them and integer constants easily, 
// we need to explictly define each value as a const

const OP_LDCONST: u8 = 0x1;  // load a constant from the chunk's const pool
const OP_RETURN:  u8 = 0xF;  // return from current function


#[repr(u8)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum OpCode {
    LoadConst  = OP_LDCONST,
    Return = OP_RETURN, 
}

impl OpCode {
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        let opcode = match byte {
            OP_LDCONST => Self::LoadConst,
            OP_RETURN => Self::Return,
            _ => return None,
        };
        Some(opcode)
    }
}

impl From<OpCode> for u8 {
    fn from(opcode: OpCode) -> Self {
        match opcode {
            OpCode::LoadConst => OP_LDCONST,
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
            Self::LoadConst => "OP_LDCONST",
            Self::Return => "OP_RETURN",
        };
        fmt.write_str(mnemonic)
    }
}

// Chunks

pub type ConstID = u16;

#[derive(Default)]
pub struct Chunk {
    bytes: Vec<u8>,
    consts: Vec<Variant>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            bytes: Vec::new(),
            consts: Vec::new(),
        }
    }
    
    pub fn bytes(&self) -> &[u8] {
        self.bytes.as_slice()
    }
    
    // using Into<u8> so that OpCodes can be accepted without extra fuss
    pub fn push_byte(&mut self, byte: impl Into<u8>) {
        self.bytes.push(byte.into());
    }
    
    pub fn extend_bytes(&mut self, bytes: &[u8]) {
        self.bytes.extend(bytes);
    }
    
    pub fn lookup_const(&self, index: impl Into<usize>) -> &Variant {
        &self.consts[index.into()]
    }
    
    pub fn push_const(&mut self, value: Variant) -> ConstID {
        let index = self.consts.len();
        self.consts.push(value);
        
        ConstID::try_from(index).expect("constant pool limit reached")
    }
}