use std::mem::size_of;

// Opcodes

// Rust enums are not like C enums! They're more like unions.
// So if we want to convert between them and integer constants easily, 
// we need to explictly define each value as a const

// 0x00-07        Control

                        // width set here so that the longest mnemonic is 16 chars
const OP_NOP:           u8 = 0x00;
const OP_RETURN:        u8 = 0x01;  // return from current function?

// The odd argument order is because the VM will swap "nargs" with "arg[n]" after reading "nargs".
// This is because the function preamble will fill in any missing default arguments such that the
// full argument list is always the same, and can be reasoned about at compile-time from the static signature.
// This wouldn't be possible if nargs was in some arbitrary location in the middle of the argument list,
// so swapping it to the beginning is the most efficient way to handle this.

// [ callobj arg[n] arg[0] ... arg[n-1] nargs ] => [ ret_value ] 
const OP_CALL:          u8 = 0x02;

// [ callobj arg[n] arg[0] ... arg[n-1] arg_seq nargs ] => [ ret_value ]  -- nargs does not include arg_seq! 
const OP_CALL_UNPACK:   u8 = 0x03;

// 0x08-40        Immediate Values

const OP_POP:           u8 = 0x08;  // [ _ ] => []
const OP_DROP:          u8 = 0x09;  // (u8); [ value[0] ... value[N] ] => []
const OP_CLONE:         u8 = 0x0A;  // [ value ] => [ value value ]

const OP_TUPLE:         u8 = 0x0B;  // (u8); [ item[0] ... item[N] ] => [ tuple ]
const OP_TUPLEN:        u8 = 0x0C;  // [ item[0] ... item[N] N ] => [ tuple ]

const OP_LD_CONST:      u8 = 0x10;  // (u8); _ => [ value ]
const OP_LD_CONST_16:   u8 = 0x11;  // (u16); _ => [ value ]
// const OP_LD_CONST_32:   u8 = 0x12;  // (u32); _ => [ value ]

const OP_IN_GLOBAL_IM:  u8 = 0x18;  // [ value name ] => [ value ]
const OP_IN_GLOBAL_MUT: u8 = 0x19;  // [ value name ] => [ value ]
const OP_ST_GLOBAL:     u8 = 0x1A;  // [ value name ] => [ value ]
const OP_LD_GLOBAL:     u8 = 0x1B;  // [ name ] => [ value ]
const OP_DP_GLOBAL:     u8 = 0x1C;  // [ name ] => []

const OP_IN_LOCAL:      u8 = 0x20;  // [ value ] => [ value ]; vm.locals += 1
const OP_ST_LOCAL:      u8 = 0x21;  // (u8);  [ value ] => [ value ]
const OP_ST_LOCAL_16:   u8 = 0x22;  // (u16); [ value ] => [ value ]
// const OP_LD_LOCAL_32:   u8 = 0x23;  // (u32); _ => [ value ]
const OP_LD_LOCAL:      u8 = 0x24;  // (u8);  _ => [ value ]
const OP_LD_LOCAL_16:   u8 = 0x25;  // (u16); _ => [ value ]
// const OP_LD_LOCAL_32:   u8 = 0x27;  // (u32); _ => [ value ]
const OP_DP_LOCALS:     u8 = 0x26;  // (u8); [ ... local[0] ... local[N] temporaries... ] => [ temporaries... ]; vm.locals -= N

// const OP_LD_NAME:       u8 = 0x28;
// const OP_LD_INDEX:      u8 = 0x29;


// Dynamic Insert/Store

// These are used to implement tuple-destructuring assignment/declaration
// const OP_IN_DYN         u8 = 0x30;  // [ value dyn_target bool ] => [] 
// const OP_ST_DYN         u8 = 0x31;  // [ value dyn_target ] => []

const OP_LD_NIL:        u8 = 0x38;  // _ => [ nil ]
const OP_LD_FALSE:      u8 = 0x39;  // _ => [ false ]
const OP_LD_TRUE:       u8 = 0x3A;  // _ => [ true ]
const OP_LD_EMPTY:      u8 = 0x3B;  // _ => [ () ]

// small numbers
const OP_LD_U8:         u8 = 0x3C;  // (u8); _ => [ value ]
const OP_LD_I8:         u8 = 0x3D;  // (i8); _ => [ value ]
const OP_LD_F8:         u8 = 0x3E;  // (i8); _ => [ value ]

// const OP_DYN_TARGET:    u8 = 0x48;  // (u8); [ ... ] => [ dyn_target ]

// 0x50-57      Unary Operations

const OP_NEG:           u8 = 0x50;  // [ operand ] => [ result ]
const OP_POS:           u8 = 0x51;
const OP_INV:           u8 = 0x52;
const OP_NOT:           u8 = 0x53;

// 0x58-60      Binary Operations

const OP_AND:           u8 = 0x58;  // [ lhs rhs ] => [ result ]
const OP_XOR:           u8 = 0x59;
const OP_OR:            u8 = 0x5A;
const OP_SHL:           u8 = 0x5B;
const OP_SHR:           u8 = 0x5C;

const OP_ADD:           u8 = 0x60;
const OP_SUB:           u8 = 0x61;
const OP_MUL:           u8 = 0x62;
const OP_DIV:           u8 = 0x63;
const OP_MOD:           u8 = 0x64;

const OP_EQ:            u8 = 0x68;
const OP_NE:            u8 = 0x69;
const OP_LT:            u8 = 0x6A;
const OP_LE:            u8 = 0x6B;
const OP_GE:            u8 = 0x6C;
const OP_GT:            u8 = 0x6D;

// 0x70-7F      Jumps

const OP_JUMP:          u8 = 0x70;  // (i16);
const OP_JUMP_FALSE:    u8 = 0x71;  // (i16); [ cond ] => [ cond ]
const OP_JUMP_TRUE:     u8 = 0x72;  // (i16); [ cond ] => [ cond ]
const OP_PJMP_FALSE:    u8 = 0x73;  // (i16); [ cond ] => []
const OP_PJMP_TRUE:     u8 = 0x74;  // (i16); [ cond ] => []

const OP_LJUMP:         u8 = 0x75;  // (i32);
const OP_LJUMP_FALSE:   u8 = 0x76;  // (i32); [ cond ] => [ cond ]
const OP_LJUMP_TRUE:    u8 = 0x77;  // (i32); [ cond ] => [ cond ]
const OP_PLJMP_FALSE:   u8 = 0x78;  // (i32); [ cond ] => []
const OP_PLJMP_TRUE:    u8 = 0x79;  // (i32); [ cond ] => []

// 0x80-8F      Iteration

// const OP_IT_INIT   // replace value with iterator state
// const OP_IT_NEXT   // replace iterator state with next state
// const OP_UNPACK:   // [ seq ] => [ item_0 ... item_n N ] -- used by unpack syntax in function calls


// 0xF0         Debugging/Tracing/Misc

const DBG_INSPECT:      u8 = 0xF0;
const DBG_ASSERT:       u8 = 0xF1;
const DBG_DUMP_STACK:   u8 = 0xF2;
const DBG_DUMP_GLOBALS: u8 = 0xF3;
const DBG_DUMP_STRINGS: u8 = 0xF4;


#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum OpCode {
    Nop = OP_NOP,
    Return = OP_RETURN, 
    Call = OP_CALL,
    CallUnpack = OP_CALL_UNPACK,
    
    Pop = OP_POP,
    Drop = OP_DROP,
    Clone = OP_CLONE,
    
    Tuple = OP_TUPLE,
    TupleN = OP_TUPLEN,
    
    LoadConst  = OP_LD_CONST,
    LoadConst16 = OP_LD_CONST_16,
    InsertGlobal = OP_IN_GLOBAL_IM,
    InsertGlobalMut = OP_IN_GLOBAL_MUT,
    StoreGlobal = OP_ST_GLOBAL,
    LoadGlobal = OP_LD_GLOBAL,
    
    InsertLocal = OP_IN_LOCAL,
    StoreLocal = OP_ST_LOCAL,
    StoreLocal16 = OP_ST_LOCAL_16,
    LoadLocal = OP_LD_LOCAL,
    LoadLocal16 = OP_LD_LOCAL_16,
    DropLocals = OP_DP_LOCALS,
    
    Nil = OP_LD_NIL,
    True = OP_LD_TRUE,
    False = OP_LD_FALSE,
    Empty = OP_LD_EMPTY,
    
    UInt8 = OP_LD_U8,
    Int8 = OP_LD_I8,
    Float8 = OP_LD_F8,
    
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
    
    Jump = OP_JUMP,
    JumpIfFalse = OP_JUMP_FALSE,
    JumpIfTrue = OP_JUMP_TRUE,
    PopJumpIfFalse = OP_PJMP_FALSE,
    PopJumpIfTrue = OP_PJMP_TRUE,
    
    LongJump = OP_LJUMP,
    LongJumpIfFalse = OP_LJUMP_FALSE,
    LongJumpIfTrue = OP_LJUMP_TRUE,
    PopLongJumpIfFalse = OP_PLJMP_FALSE,
    PopLongJumpIfTrue = OP_PLJMP_TRUE,
    
    Inspect = DBG_INSPECT,
    Assert = DBG_ASSERT,
}

impl OpCode {
    #[inline]
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        let opcode = match byte {
            OP_NOP => Self::Nop,
            OP_RETURN => Self::Return,
            OP_CALL => Self::Call,
            OP_CALL_UNPACK => Self::CallUnpack,
            
            OP_POP => Self::Pop,
            OP_DROP => Self::Drop,
            OP_CLONE => Self::Clone,
            
            OP_TUPLE => Self::Tuple,
            OP_TUPLEN => Self::TupleN,
            
            OP_LD_CONST => Self::LoadConst,
            OP_LD_CONST_16 => Self::LoadConst16,
            
            OP_IN_GLOBAL_IM => Self::InsertGlobal,
            OP_IN_GLOBAL_MUT => Self::InsertGlobalMut,
            OP_ST_GLOBAL => Self::StoreGlobal,
            OP_LD_GLOBAL => Self::LoadGlobal,
            
            OP_IN_LOCAL => Self::InsertLocal,
            OP_ST_LOCAL => Self::StoreLocal,
            OP_ST_LOCAL_16 => Self::StoreLocal16,
            OP_LD_LOCAL => Self::LoadLocal,
            OP_LD_LOCAL_16 => Self::LoadLocal16,
            OP_DP_LOCALS => Self::DropLocals,
            
            OP_LD_NIL => Self::Nil,
            OP_LD_TRUE => Self::True,
            OP_LD_FALSE => Self::False,
            OP_LD_EMPTY => Self::Empty,
            OP_LD_U8 => Self::UInt8,
            OP_LD_I8 => Self::Int8,
            OP_LD_F8 => Self::Float8,
            
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
            
            OP_JUMP => Self::Jump,
            OP_JUMP_FALSE => Self::JumpIfFalse,
            OP_JUMP_TRUE => Self::JumpIfTrue,
            OP_PJMP_FALSE => Self::PopJumpIfFalse,
            OP_PJMP_TRUE => Self::PopJumpIfTrue,
            
            OP_LJUMP => Self::LongJump,
            OP_LJUMP_FALSE => Self::LongJumpIfFalse,
            OP_LJUMP_TRUE => Self::LongJumpIfTrue,
            OP_PLJMP_FALSE => Self::PopLongJumpIfFalse,
            OP_PLJMP_TRUE => Self::PopLongJumpIfTrue,
            
            DBG_INSPECT => Self::Inspect,
            DBG_ASSERT => Self::Assert,
            
            _ => return None,
        };
        Some(opcode)
    }
    
    #[inline]
    pub const fn instr_len(&self) -> usize {
        match self {
            // don't really need size_of() for most of these, but it's a nice little bit of self-documentation

            Self::Drop           => 1 + size_of::<u8>(),
            
            Self::LoadConst      => 1 + size_of::<u8>(),
            Self::LoadConst16    => 1 + size_of::<u16>(),
            
            Self::StoreLocal     => 1 + size_of::<u8>(),
            Self::StoreLocal16   => 1 + size_of::<u16>(),
            Self::LoadLocal      => 1 + size_of::<u8>(),
            Self::LoadLocal16    => 1 + size_of::<u16>(),
            Self::DropLocals     => 1 + size_of::<u8>(),
            
            Self::Tuple          => 1 + size_of::<u8>(),
            Self::UInt8          => 1 + size_of::<u8>(),
            Self::Int8           => 1 + size_of::<i8>(),
            Self::Float8         => 1 + size_of::<i8>(),
            
            Self::Jump           => 1 + size_of::<i16>(),
            Self::JumpIfFalse    => 1 + size_of::<i16>(),
            Self::JumpIfTrue     => 1 + size_of::<i16>(),
            Self::PopJumpIfFalse => 1 + size_of::<i16>(),
            Self::PopJumpIfTrue  => 1 + size_of::<i16>(),
            
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
            Self::Nop => "NOP",
            Self::Return => "RETURN",
            Self::Call => "CALL",
            Self::CallUnpack => "CALL_UNPACK",
            
            Self::Pop => "POP",
            Self::Drop => "DROP",
            Self::Clone => "CLONE",
            
            Self::Tuple => "TUPLE",
            Self::TupleN => "TUPLEN",
            
            Self::LoadConst => "LD_CONST",
            Self::LoadConst16 => "LD_CONST_16",
            
            Self::InsertGlobal => "IN_GLOBAL_IM",
            Self::InsertGlobalMut => "IN_GLOBAL_MUT",
            Self::StoreGlobal => "ST_GLOBAL",
            Self::LoadGlobal => "LD_GLOBAL",
            
            Self::InsertLocal => "IN_LOCAL",
            Self::StoreLocal => "ST_LOCAL",
            Self::StoreLocal16 => "ST_LOCAL_16",
            Self::LoadLocal => "LD_LOCAL",
            Self::LoadLocal16 => "LD_LOCAL_16",
            Self::DropLocals => "DP_LOCALS",
            
            Self::Nil => "LD_NIL",
            Self::True => "LD_TRUE",
            Self::False => "LD_FALSE",
            Self::Empty => "LD_EMPTY",
            Self::UInt8 => "LD_U8",
            Self::Int8 => "LD_I8",
            Self::Float8 => "LD_F8",
            
            Self::Neg => "NEG",
            Self::Pos => "POS",
            Self::Inv => "INV",
            Self::Not => "NOT",
            
            Self::And => "AND",
            Self::Xor => "XOR",
            Self::Or =>  "OR",
            Self::Shl => "SHL",
            Self::Shr => "SHR",
            Self::Add => "ADD",
            Self::Sub => "SUB",
            Self::Mul => "MUL",
            Self::Div => "DIV",
            Self::Mod => "MOD",
            Self::EQ => "CMP_EQ",
            Self::NE => "CMP_NE",
            Self::LT => "CMP_LT",
            Self::LE => "CMP_LE",
            Self::GE => "CMP_GE",
            Self::GT => "CMP_GT",
            
            Self::Jump => "JUMP",
            Self::JumpIfFalse => "JUMP_FALSE",
            Self::JumpIfTrue => "JUMP_TRUE",
            Self::PopJumpIfFalse => "PJMP_FALSE",
            Self::PopJumpIfTrue => "PJMP_TRUE",
            
            Self::LongJump => "LJUMP",
            Self::LongJumpIfFalse => "LJUMP_FALSE",
            Self::LongJumpIfTrue => "LJUMP_TRUE",
            Self::PopLongJumpIfFalse => "PLJMP_FALSE",
            Self::PopLongJumpIfTrue => "PLJMP_TRUE",
            
            Self::Inspect => "DBG_INSPECT",
            Self::Assert => "DBG_ASSERT",
        };
        
        if let Some(width) = fmt.width() {
            write!(fmt, "{:1$}", mnemonic, width)
        } else {
            fmt.write_str(mnemonic)
        }
    }
}