// Opcodes

// Rust enums are not like C enums! They're more like unions.
// So if we want to convert between them and integer constants easily, 
// we need to explictly define each value as a const

// 0x00         Control

                        // width set here so that the longest mnemonic is 16 chars
const OP_RETURN:        u8 = 0x00;  // return from current function?
const OP_EXIT:          u8 = 0x01;

// 0x10-30        Immediate Values

const OP_POP:           u8 = 0x10;  // [ _ ] => []

const OP_LD_CONST:      u8 = 0x21;  // (u8); _ => [ value ]
const OP_LD_CONST_16:   u8 = 0x22;  // (u16); _ => [ value ]

const OP_IN_GLOBAL_IM:  u8 = 0x23;  // [ value name ] => [ value ]
const OP_IN_GLOBAL_MUT: u8 = 0x24;  // [ value name ] => [ value ]
const OP_ST_GLOBAL:     u8 = 0x26;  // [ value name ] => [ value ]
const OP_LD_GLOBAL:     u8 = 0x27;  // [ name ] => [ value ]

// const OP_IN_LOCAL:      u8 = 0x27;  // Note: local mutability tracking is done by the compiler
// const OP_ST_LOCAL:      u8 = 0x28;
// const OP_ST_LOCAL_16:   u8 = 0x29;
// const OP_LD_LOCAL:      u8 = 0x2A;
// const OP_LD_LOCAL_16:   u8 = 0x2B;

// const OP_LD_NAME:       u8 = 0x2C;
// const OP_LD_INDEX:      u8 = 0x2D;

// Dynamic Insert/Store

// const OP_IN_DYN         u8 = ...;  // [ target: tuple, value, mut: bool] => []

const OP_NIL:           u8 = 0x30;  // _ => [ nil ]
const OP_FALSE:         u8 = 0x31;  // _ => [ false ]
const OP_TRUE:          u8 = 0x32;  // _ => [ true ]
const OP_EMPTY:         u8 = 0x33;  // _ => [ () ]
const OP_TUPLE:         u8 = 0x34;  // (u8); [ ... ] => [ tuple ]

// 0x40         Unary Operations

const OP_NEG:           u8 = 0x40;  // [ operand ] => [ result ]
const OP_POS:           u8 = 0x41;
const OP_INV:           u8 = 0x42;
const OP_NOT:           u8 = 0x43;

// 0x50-60      Binary Operations

const OP_AND:           u8 = 0x50;  // [ lhs rhs ] => [ result ]
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
const DBG_DUMP_STACK:   u8 = 0xF1;
const DBG_DUMP_GLOBALS: u8 = 0xF2;
const DBG_DUMP_STRINGS: u8 = 0xF3;


#[repr(u8)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum OpCode {
    Return = OP_RETURN, 
    
    Pop = OP_POP,
    LoadConst  = OP_LD_CONST,
    LoadConst16 = OP_LD_CONST_16,
    InsertGlobal = OP_IN_GLOBAL_IM,
    InsertGlobalMut = OP_IN_GLOBAL_MUT,
    StoreGlobal = OP_ST_GLOBAL,
    LoadGlobal = OP_LD_GLOBAL,
    
    Nil = OP_NIL,
    True = OP_TRUE,
    False = OP_FALSE,
    Empty = OP_EMPTY,
    Tuple = OP_TUPLE,
    
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
}

impl OpCode {
    #[inline]
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        let opcode = match byte {
            OP_RETURN => Self::Return,
            
            OP_POP => Self::Pop,
            OP_LD_CONST => Self::LoadConst,
            OP_LD_CONST_16 => Self::LoadConst16,
            OP_IN_GLOBAL_IM => Self::InsertGlobal,
            OP_IN_GLOBAL_MUT => Self::InsertGlobalMut,
            OP_ST_GLOBAL => Self::StoreGlobal,
            OP_LD_GLOBAL => Self::LoadGlobal,
            
            OP_NIL => Self::Nil,
            OP_TRUE => Self::True,
            OP_FALSE => Self::False,
            OP_EMPTY => Self::Empty,
            OP_TUPLE => Self::Tuple,
            
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
            
            _ => return None,
        };
        Some(opcode)
    }
    
    #[inline]
    pub fn instr_len(&self) -> usize {
        match self {
            Self::LoadConst => 2,
            Self::LoadConst16 => 3,
            Self::Tuple => 2,
            _ => 1,
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
            Self::InsertGlobal => "OP_IN_GLOBAL_IM",
            Self::InsertGlobalMut => "OP_IN_GLOBAL_MUT",
            Self::StoreGlobal => "OP_ST_GLOBAL",
            Self::LoadGlobal => "OP_LD_GLOBAL",
            
            Self::Nil => "OP_NIL",
            Self::True => "OP_TRUE",
            Self::False => "OP_FALSE",
            Self::Empty => "OP_EMPTY",
            Self::Tuple => "OP_TUPLE",
            
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
        };
        
        if let Some(width) = fmt.width() {
            write!(fmt, "{:1$}", mnemonic, width)
        } else {
            fmt.write_str(mnemonic)
        }
    }
}