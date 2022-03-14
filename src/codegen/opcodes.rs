// Opcodes

// Rust enums are not like C enums! They're more like unions.
// So if we want to convert between them and integer constants easily, 
// we need to explictly define each value as a const

// 0x00         Control

                        // width set here so that the longest mnemonic is 16 chars
const OP_RETURN:        u8 = 0x00;  // return from current function?
const OP_EXIT:          u8 = 0x01;

// 0x10-30        Immediate Values

const OP_POP:           u8 = 0x10;

const OP_LD_CONST:      u8 = 0x21;  // load a constant from the chunk's const pool
const OP_LD_CONST_16:   u8 = 0x22;  // ...using a 16-bit index

const OP_CR_GLOBAL_IM:  u8 = 0x23;
const OP_CR_GLOBAL_MUT: u8 = 0x24;
const OP_ST_GLOBAL:     u8 = 0x25;
const OP_ST_GLOBAL_16:  u8 = 0x26;
const OP_LD_GLOBAL:     u8 = 0x27;
const OP_LD_GLOBAL_16:  u8 = 0x28;

const OP_CR_LOCAL:      u8 = 0x29;  // Note: local mutability tracking is done by the compiler
const OP_ST_LOCAL:      u8 = 0x2A;
const OP_ST_LOCAL_16:   u8 = 0x2B;
const OP_LD_LOCAL:      u8 = 0x2C;
const OP_LD_LOCAL_16:   u8 = 0x2D;

const OP_LD_NAME:       u8 = 0x2E;
const OP_LD_INDEX:      u8 = 0x2F;

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


// 0xF0         Debugging/Tracing/Misc

const DBG_INSPECT:      u8 = 0xF0;
const DBG_DUMP:         u8 = 0xF1;


#[repr(u8)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum OpCode {
    Return = OP_RETURN, 
    
    Pop = OP_POP,
    LoadConst  = OP_LD_CONST,
    LoadConst16 = OP_LD_CONST_16,
    
    Nil = OP_NIL,
    Empty = OP_EMPTY,
    True = OP_TRUE,
    False = OP_FALSE,
    
    Neg = OP_NEG,
    Pos = OP_POS,
    Inv = OP_INV,
    Not = OP_NOT,
    
    And = OP_AND,
    Xor = OP_XOR,
    Or = OP_OR,
    Shl = OP_SHL,
    Shr = OP_SHR,
    Add = OP_ADD,
    Sub = OP_SUB,
    Mul = OP_MUL,
    Div = OP_DIV,
    Mod = OP_MOD,
    EQ = OP_EQ,
    NE = OP_NE,
    LT = OP_LT,
    LE = OP_LE,
    GE = OP_GE,
    GT = OP_GT,
    
    Inspect = DBG_INSPECT,
    Dump = DBG_DUMP,
}

impl OpCode {
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        let opcode = match byte {
            OP_RETURN => Self::Return,
            
            OP_POP => Self::Pop,
            OP_LD_CONST => Self::LoadConst,
            OP_LD_CONST_16 => Self::LoadConst16,
            
            OP_NIL => Self::Nil,
            OP_EMPTY => Self::Empty,
            OP_TRUE => Self::True,
            OP_FALSE => Self::False,
            
            OP_NEG => Self::Neg,
            OP_POS => Self::Pos,
            OP_INV => Self::Inv,
            OP_NOT => Self::Not,
            
            OP_AND => Self::And,
            OP_XOR => Self::Xor,
            OP_OR => Self::Or,
            OP_SHL => Self::Shl,
            OP_SHR => Self::Shr,
            OP_ADD => Self::Add,
            OP_SUB => Self::Sub,
            OP_MUL => Self::Mul,
            OP_DIV => Self::Div,
            OP_MOD => Self::Mod,
            OP_EQ => Self::EQ,
            OP_NE => Self::NE,
            OP_LT => Self::LT,
            OP_LE => Self::LE,
            OP_GE => Self::GE,
            OP_GT => Self::GT,
            
            DBG_INSPECT => Self::Inspect,
            DBG_DUMP => Self::Dump,
            
            _ => return None,
        };
        Some(opcode)
    }
    
    pub fn instr_len(&self) -> usize {
        match self {
            Self::Return => 1,
            
            Self::Pop => 1,
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
            
            Self::And => 1,
            Self::Xor => 1,
            Self::Or => 1,
            Self::Shl => 1,
            Self::Shr => 1,
            Self::Add => 1,
            Self::Sub => 1,
            Self::Mul => 1,
            Self::Div => 1,
            Self::Mod => 1,
            Self::EQ => 1,
            Self::NE => 1,
            Self::LT => 1,
            Self::LE => 1,
            Self::GE => 1,
            Self::GT => 1,
            
            Self::Inspect => 1,
            Self::Dump => 1,
        }
    }
}

impl From<OpCode> for u8 {
    fn from(opcode: OpCode) -> Self { opcode as u8 }
}

impl PartialEq<u8> for OpCode {
    fn eq(&self, other: &u8) -> bool { *other == (*self).into() }
}

// For disassembly/debugging
impl std::fmt::Display for OpCode {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mnemonic = match *self {
            Self::Return => "OP_RETURN",
            
            Self::Pop => "OP_POP",
            Self::LoadConst => "OP_LD_CONST",
            Self::LoadConst16 => "OP_LD_CONST_16",
            
            Self::Nil => "OP_NIL",
            Self::Empty => "OP_EMPTY",
            Self::True => "OP_TRUE",
            Self::False => "OP_FALSE",
            
            Self::Neg => "OP_NEG",
            Self::Pos => "OP_POS",
            Self::Inv => "OP_INV",
            Self::Not => "OP_NOT",
            
            Self::And => "OP_AND",
            Self::Xor => "OP_XOR",
            Self::Or => "OP_OR",
            Self::Shl => "OP_SHL",
            Self::Shr => "OP_SHR",
            Self::Add => "OP_ADD",
            Self::Sub => "OP_SUB",
            Self::Mul => "OP_MUL",
            Self::Div => "OP_DIV",
            Self::Mod => "OP_MOD",
            Self::EQ => "OP_EQ",
            Self::NE => "OP_NE",
            Self::LT => "OP_LT",
            Self::LE => "OP_LE",
            Self::GE => "OP_GE",
            Self::GT => "OP_GT",
            
            Self::Inspect => "DBG_INSPECT",
            Self::Dump => "DBG_DUMP",
        };
        
        if let Some(width) = fmt.width() {
            write!(fmt, "{:1$}", mnemonic, width)
        } else {
            fmt.write_str(mnemonic)
        }
    }
}