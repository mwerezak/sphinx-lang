// Opcodes

// Rust enums are not like C enums! They're more like unions.
// So if we want to convert between them and integer constants easily, 
// we need to explictly define each value as a const

// 0x00         Control

const OP_RETURN:        u8 = 0x00;  // return from current function

// 0x20-30        Immediate Values

const OP_LDCONST:       u8 = 0x20;  // load a constant from the chunk's const pool
const OP_LDCONST_16:    u8 = 0x21;  // ...using a 16-bit index

const OP_NIL:           u8 = 0x30;
const OP_EMPTY:         u8 = 0x31;
const OP_TRUE:          u8 = 0x32;
const OP_FALSE:         u8 = 0x33;

// 0x40         Unary Operations

const OP_NEG:           u8 = 0x40;
const OP_POS:           u8 = 0x41;
const OP_INV:           u8 = 0x42;
const OP_NOT:           u8 = 0x43;

// 0x50-60      Binary Operations

const OP_AND:           u8 = 0x50;
const OP_XOR:           u8 = 0x51;
const OP_OR:            u8 = 0x52;

const OP_SHL:           u8 = 0x53;
const OP_SHR:           u8 = 0x54;

const OP_ADD:           u8 = 0x55;
const OP_SUB:           u8 = 0x56;
const OP_MUL:           u8 = 0x57;
const OP_DIV:           u8 = 0x58;
const OP_MOD:           u8 = 0x59;

const OP_EQ:            u8 = 0x5A;
const OP_NE:            u8 = 0x5B;

const OP_LT:            u8 = 0x5C;
const OP_LE:            u8 = 0x5D;
const OP_GE:            u8 = 0x5E;
const OP_GT:            u8 = 0x5F;

// 0x70         Jumps



#[repr(u8)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum OpCode {
    Return = OP_RETURN, 
    
    LoadConst  = OP_LDCONST,
    LoadConst16 = OP_LDCONST_16,
    
    Nil = OP_NIL,
    Empty = OP_EMPTY,
    True = OP_TRUE,
    False = OP_FALSE,
    
    Neg = OP_NEG,
    Pos = OP_POS,
    Inv = OP_INV,
    Not = OP_NOT,
}

impl OpCode {
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        let opcode = match byte {
            OP_RETURN => Self::Return,
            
            OP_LDCONST => Self::LoadConst,
            OP_LDCONST_16 => Self::LoadConst16,
            
            OP_NIL => Self::Nil,
            OP_EMPTY => Self::Empty,
            OP_TRUE => Self::True,
            OP_FALSE => Self::False,
            
            OP_NEG => Self::Neg,
            OP_POS => Self::Pos,
            OP_INV => Self::Inv,
            OP_NOT => Self::Not,
            
            _ => return None,
        };
        Some(opcode)
    }
    
    pub fn instr_len(&self) -> usize {
        match self {
            Self::Return => 1,
            
            Self::LoadConst => 2,
            Self::LoadConst16 => 3,
            
            Self::Nil => 1,
            Self::Empty => 1,
            Self::True => 1,
            Self::False => 1,
            
            Self::Neg => 1,
            Self::Pos => 1,
            Self::Inv => 1,
            Self::Not => 1,
        }
    }
}

impl From<OpCode> for u8 {
    fn from(opcode: OpCode) -> Self { opcode as u8 }
}

impl PartialEq<u8> for OpCode {
    fn eq(&self, other: &u8) -> bool { *other == (*self).into() }
}

