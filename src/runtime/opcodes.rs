use crate::runtime::variant::Variant;

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
