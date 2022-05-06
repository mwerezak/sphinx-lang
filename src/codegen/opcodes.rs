use core::mem::size_of;


pub type LocalIndex = u16;
pub type UpvalueIndex = u16;


// Opcodes

// Rust enums are not like C enums! They're more like unions.
// So if we want to convert between them and integer constants easily, 
// we need to explictly define each value as a const

// 0x00-07        Control

                           // width set here so that the longest mnemonic is 16 chars
const OP_NOP:              u8 = 0x00;
const OP_EXIT:             u8 = 0x01;  // _ => !
const OP_ERROR:            u8 = 0x02;  // [ error ] => !

const OP_RETURN:           u8 = 0x08;  // [ ...call frame... ret_value ] => [ ret_value ]

// The odd argument order is because the VM will swap "nargs" with "arg[n]" after reading "nargs".
// This is because the function preamble will fill in any missing default arguments such that the
// full argument list is always the same, and can be reasoned about at compile-time from the static signature.
// This wouldn't be possible if nargs was in some arbitrary location in the middle of the argument list,
// so swapping it to the beginning is the most efficient way to handle this.

// [ callee arg[n] arg[0] ... arg[n-1] nargs ] => [ ret_value ] 
const OP_CALL:             u8 = 0x09;

// This instruction is needed because when unpacking, the final nargs value needs to be updated dynamically as we iterate.
// [ callee arg[n] arg[0] ... arg[n-1] nargs arg_seq ] => [ ret_value ]  -- nargs does not include arg_seq! 
const OP_CALL_UNPACK:      u8 = 0x0A;

// 0x10-17        Immediate Values

const OP_POP:              u8 = 0x10;  // [ _ ] => []
const OP_DROP:             u8 = 0x11;  // (u8); [ value[0] ... value[N] ] => []
const OP_CLONE:            u8 = 0x12;  // [ value ] => [ value value ]
const OP_SWAP:             u8 = 0x13;  // (u8); [ value[A] ... value[B] ] => [ value[B] ... value[A] ]
const OP_SHIFT:            u8 = 0x14;  // (u8); [ value[A] ... value[B] ] => [ ... value[B] value[A] ]

const OP_TUPLE:            u8 = 0x18;  // (u8); [ item[0] ... item[N] ] => [ tuple ]
const OP_TUPLEN:           u8 = 0x19;  // [ item[0] ... item[N] N ] => [ tuple ]

// 0x18-1F        Iteration

const OP_ITER_INIT:        u8 = 0x1A;  // [ iterable ] => [ iter state[0] ]
const OP_ITER_NEXT:        u8 = 0x1B;  // [ iter state[N] ] => [ iter state[N+1] value[N] ]
const OP_ITER_UNPACK:      u8 = 0x1C;  // [ iterable ] => [ value[0] ... value[N] ]

// 0x40-5F        Load/Store

const OP_LD_FUN:           u8 = 0x40;  // (u8);  _ => [ function ]
const OP_LD_FUN_16:        u8 = 0x41;  // (u16); _ => [ function ]

const OP_LD_CONST:         u8 = 0x42;  // (u8);  _ => [ value ]
const OP_LD_CONST_16:      u8 = 0x43;  // (u16); _ => [ value ]
// const OP_LD_CONST_32:   u8 = 0x44;  // (u32); _ => [ value ]

const OP_IN_GLOBAL_IM:     u8 = 0x48;  // [ value name ] => [ value ]
const OP_IN_GLOBAL_MUT:    u8 = 0x49;  // [ value name ] => [ value ]
const OP_ST_GLOBAL:        u8 = 0x4A;  // [ value name ] => [ value ]
const OP_LD_GLOBAL:        u8 = 0x4B;  // [ name ] => [ value ]
const OP_DP_GLOBAL:        u8 = 0x4C;  // [ name ] => []

const OP_IN_LOCAL:         u8 = 0x50;  // [ value ] => [ value ]; vm.locals += 1
const OP_ST_LOCAL:         u8 = 0x51;  // (u8);  [ value ] => [ value ]
const OP_ST_LOCAL_16:      u8 = 0x52;  // (u16); [ value ] => [ value ]
const OP_LD_LOCAL:         u8 = 0x53;  // (u8);  _ => [ value ]
const OP_LD_LOCAL_16:      u8 = 0x54;  // (u16); _ => [ value ]
const OP_DP_LOCALS:        u8 = 0x55;  // (u8); [ local[0] ... local[N] temporaries... ] => [ temporaries... ]; vm.locals -= N

const OP_ST_UPVAL:         u8 = 0x58;  // (u8);  [ value ] => [ value ]
const OP_ST_UPVAL_16:      u8 = 0x59;  // (u16); [ value ] => [ value ]
const OP_LD_UPVAL:         u8 = 0x5A;  // (u8);  _ => [ value ]
const OP_LD_UPVAL_16:      u8 = 0x5B;  // (u16); _ => [ value ]

const OP_CLOSE_UPVAL:      u8 = 0x5C;  // (u8);
const OP_CLOSE_UPVAL_16:   u8 = 0x5D;  // (u16);

// 0x60-F      Values

const OP_LD_NIL:           u8 = 0x60;  // _ => [ nil ]
const OP_LD_FALSE:         u8 = 0x61;  // _ => [ false ]
const OP_LD_TRUE:          u8 = 0x62;  // _ => [ true ]
const OP_LD_EMPTY:         u8 = 0x63;  // _ => [ () ]

// small numbers
const OP_LD_U8:            u8 = 0x64;  // (u8); _ => [ value ]
const OP_LD_I8:            u8 = 0x65;  // (i8); _ => [ value ]
const OP_LD_I16:           u8 = 0x66;  // (i16); _ => [ value ]

// 0x70-77      Unary Operations

const OP_NEG:              u8 = 0x70;  // [ operand ] => [ result ]
const OP_POS:              u8 = 0x71;
const OP_INV:              u8 = 0x72;
const OP_NOT:              u8 = 0x73;

// 0x78-8F      Binary Operations

const OP_AND:              u8 = 0x78;  // [ lhs rhs ] => [ result ]
const OP_XOR:              u8 = 0x79;
const OP_OR:               u8 = 0x7A;
const OP_SHL:              u8 = 0x7B;
const OP_SHR:              u8 = 0x7C;

const OP_ADD:              u8 = 0x80;
const OP_SUB:              u8 = 0x81;
const OP_MUL:              u8 = 0x82;
const OP_DIV:              u8 = 0x83;
const OP_MOD:              u8 = 0x84;

const OP_EQ:               u8 = 0x88;
const OP_NE:               u8 = 0x89;
const OP_LT:               u8 = 0x8A;
const OP_LE:               u8 = 0x8B;
const OP_GE:               u8 = 0x8C;
const OP_GT:               u8 = 0x8D;

// 0x90-9F      Jumps

const OP_JUMP:             u8 = 0x90;  // (i16);
const OP_JUMP_FALSE:       u8 = 0x91;  // (i16); [ cond ] => [ cond ]
const OP_JUMP_TRUE:        u8 = 0x92;  // (i16); [ cond ] => [ cond ]
const OP_PJMP_FALSE:       u8 = 0x93;  // (i16); [ cond ] => []
const OP_PJMP_TRUE:        u8 = 0x94;  // (i16); [ cond ] => []

const OP_LJUMP:            u8 = 0x98;  // (i32);
const OP_LJUMP_FALSE:      u8 = 0x99;  // (i32); [ cond ] => [ cond ]
const OP_LJUMP_TRUE:       u8 = 0x9A;  // (i32); [ cond ] => [ cond ]
const OP_PLJMP_FALSE:      u8 = 0x9B;  // (i32); [ cond ] => []
const OP_PLJMP_TRUE:       u8 = 0x9C;  // (i32); [ cond ] => []

// 0xF0-FF      Debugging/Tracing/Misc

const DBG_INSPECT:         u8 = 0xF0;
const DBG_ASSERT:          u8 = 0xF1;
const DBG_DUMP_STACK:      u8 = 0xF2;
const DBG_DUMP_GLOBALS:    u8 = 0xF3;
const DBG_DUMP_STRINGS:    u8 = 0xF4;


#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum OpCode {
    Nop = OP_NOP,
    Exit = OP_EXIT,
    Error = OP_ERROR,
    
    Return = OP_RETURN, 
    Call = OP_CALL,
    CallUnpack = OP_CALL_UNPACK,
    
    Pop = OP_POP,
    Drop = OP_DROP,
    Clone = OP_CLONE,
    
    Tuple = OP_TUPLE,
    TupleN = OP_TUPLEN,
    
    IterInit = OP_ITER_INIT,
    IterNext = OP_ITER_NEXT,
    
    LoadFunction = OP_LD_FUN,
    LoadFunction16 = OP_LD_FUN_16,
    
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
    
    StoreUpvalue = OP_ST_UPVAL,
    StoreUpvalue16 = OP_ST_UPVAL_16,
    LoadUpvalue = OP_LD_UPVAL,
    LoadUpvalue16 = OP_LD_UPVAL_16,
    
    CloseUpvalue = OP_CLOSE_UPVAL,
    CloseUpvalue16 = OP_CLOSE_UPVAL_16,
    
    Nil = OP_LD_NIL,
    True = OP_LD_TRUE,
    False = OP_LD_FALSE,
    Empty = OP_LD_EMPTY,
    
    UInt8 = OP_LD_U8,
    Int8 = OP_LD_I8,
    Int16 = OP_LD_I16,
    
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
            OP_EXIT => Self::Exit,
            OP_ERROR => Self::Error,
            
            OP_RETURN => Self::Return,
            OP_CALL => Self::Call,
            OP_CALL_UNPACK => Self::CallUnpack,
            
            OP_POP => Self::Pop,
            OP_DROP => Self::Drop,
            OP_CLONE => Self::Clone,
            
            OP_TUPLE => Self::Tuple,
            OP_TUPLEN => Self::TupleN,
            
            OP_ITER_INIT => Self::IterInit,
            OP_ITER_NEXT => Self::IterNext,
            
            OP_LD_FUN => Self::LoadFunction,
            OP_LD_FUN_16 => Self::LoadFunction16,
            
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
            
            OP_ST_UPVAL => Self::StoreUpvalue,
            OP_ST_UPVAL_16 => Self::StoreUpvalue16,
            OP_LD_UPVAL => Self::LoadUpvalue,
            OP_LD_UPVAL_16 => Self::LoadUpvalue16,
            
            OP_CLOSE_UPVAL => Self::CloseUpvalue,
            OP_CLOSE_UPVAL_16 => Self::CloseUpvalue16,
            
            OP_LD_NIL => Self::Nil,
            OP_LD_TRUE => Self::True,
            OP_LD_FALSE => Self::False,
            OP_LD_EMPTY => Self::Empty,
            OP_LD_U8 => Self::UInt8,
            OP_LD_I8 => Self::Int8,
            OP_LD_I16 => Self::Int16,
            
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
            
            Self::LoadFunction   => 1 + size_of::<u8>(),
            Self::LoadFunction16 => 1 + size_of::<u16>(),
            
            Self::LoadConst      => 1 + size_of::<u8>(),
            Self::LoadConst16    => 1 + size_of::<u16>(),
            
            Self::StoreLocal     => 1 + size_of::<u8>(),
            Self::StoreLocal16   => 1 + size_of::<u16>(),
            Self::LoadLocal      => 1 + size_of::<u8>(),
            Self::LoadLocal16    => 1 + size_of::<u16>(),
            Self::DropLocals     => 1 + size_of::<u8>(),
            
            Self::StoreUpvalue   => 1 + size_of::<u8>(),
            Self::StoreUpvalue16 => 1 + size_of::<u16>(),
            Self::LoadUpvalue    => 1 + size_of::<u8>(),
            Self::LoadUpvalue16  => 1 + size_of::<u16>(),
            
            Self::CloseUpvalue   => 1 + size_of::<u8>(),
            Self::CloseUpvalue16 => 1 + size_of::<u16>(),
            
            Self::Tuple          => 1 + size_of::<u8>(),
            Self::UInt8          => 1 + size_of::<u8>(),
            Self::Int8           => 1 + size_of::<i8>(),
            Self::Int16          => 1 + size_of::<i16>(),
            
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

impl TryFrom<u8> for OpCode {
    type Error = u8;
    fn try_from(byte: u8) -> Result<Self, u8> {
        if let Some(opcode) = OpCode::from_byte(byte) {
            Ok(opcode)
        } else {
            Err(byte)
        }
    }
}

impl PartialEq<u8> for OpCode {
    fn eq(&self, other: &u8) -> bool { *other == (*self).into() }
}

// For disassembly/debugging
impl core::fmt::Display for OpCode {
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mnemonic = match *self {
            Self::Nop => "NOP",
            Self::Exit => "EXIT",
            Self::Error => "ERROR",
            
            Self::Return => "RETURN",
            Self::Call => "CALL",
            Self::CallUnpack => "CALL_UNPACK",
            
            Self::Pop => "POP",
            Self::Drop => "DROP",
            Self::Clone => "CLONE",
            
            Self::Tuple => "TUPLE",
            Self::TupleN => "TUPLEN",
            
            Self::IterInit => "ITER_INIT",
            Self::IterNext => "ITER_NEXT",
            
            Self::LoadFunction => "OP_LD_FUN",
            Self::LoadFunction16 => "OP_LD_FUN_16",
            
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
            
            Self::StoreUpvalue => "ST_UPVAL",
            Self::StoreUpvalue16 => "ST_UPVAL_16",
            Self::LoadUpvalue => "LD_UPVAL",
            Self::LoadUpvalue16 => "LD_UPVAL_16",
            
            Self::CloseUpvalue => "CLOSE_UPVAL",
            Self::CloseUpvalue16 => "CLOSE_UPVAL_16",
            
            Self::Nil => "LD_NIL",
            Self::True => "LD_TRUE",
            Self::False => "LD_FALSE",
            Self::Empty => "LD_EMPTY",
            Self::UInt8 => "LD_U8",
            Self::Int8 => "LD_I8",
            Self::Int16 => "LD_I16",
            
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