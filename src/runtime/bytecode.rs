use crate::runtime::variant::Variant;

// Opcodes

// Rust enums are not like C enums! They're more like unions.
// So if we want to convert between them and integer constants easily, 
// we need to explictly define each value as a const

const OP_RETURN:        u8 = 0x0;  // return from current function
const OP_LDCONST:       u8 = 0x1;  // load a constant from the chunk's const pool
const OP_LDCONST_16:    u8 = 0x2;  // ...using a 16-bit index

const OP_NIL:           u8 = 0x8;
const OP_EMPTY:         u8 = 0x9;
const OP_TRUE:          u8 = 0xA;
const OP_FALSE:         u8 = 0xB;

// Operators

const OP_NEG:           u8 = 0x10;
const OP_POS:           u8 = 0x11;
const OP_INV:           u8 = 0x12;
const OP_NOT:           u8 = 0x13;

const OP_AND:           u8 = 0x14;
const OP_XOR:           u8 = 0x15;
const OP_OR:            u8 = 0x16;

const OP_SHL:           u8 = 0x17;
const OP_SHR:           u8 = 0x18;

const OP_ADD:           u8 = 0x19;
const OP_MUL:           u8 = 0x1A;
const OP_DIV:           u8 = 0x1B;
const OP_MOD:           u8 = 0x1C;

const OP_EQ:            u8 = 0x1D;

const OP_LT:            u8 = 0x1E;
const OP_LE:            u8 = 0x1F;
const OP_GE:            u8 = 0x20;
const OP_GT:            u8 = 0x21;




#[repr(u8)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum OpCode {
    LoadConst  = OP_LDCONST,
    LoadConstWide = OP_LDCONST_16,
    Return = OP_RETURN, 
}

impl OpCode {
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        let opcode = match byte {
            OP_LDCONST => Self::LoadConst,
            OP_LDCONST_16 => Self::LoadConstWide,
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
            OpCode::LoadConstWide => OP_LDCONST_16,
            OpCode::Return => OP_RETURN,
        }
    }
}

impl PartialEq<u8> for OpCode {
    fn eq(&self, other: &u8) -> bool {
        *other == (*self).into()
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