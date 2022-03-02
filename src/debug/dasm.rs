use std::fmt;
use std::fmt::Formatter;
use crate::runtime::variant::Variant;
use crate::runtime::bytecode::{Chunk, OpCode, ConstID};
use crate::debug::symbol::ChunkDebugSymbols;


pub struct Disassembler<'c> {
    chunk: &'c Chunk,
    symbols: Option<ChunkDebugSymbols>,
}

impl<'c> Disassembler<'c> {
    pub fn new(chunk: &'c Chunk) -> Self {
        Disassembler { chunk, symbols: None }
    }
    
    pub fn with_symbols(mut self, symbols: ChunkDebugSymbols) -> Self {
        self.symbols = Some(symbols); self
    }
    
    fn decode_chunk(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        let mut offset = 0;
        while offset < self.chunk.bytes().len() {
            let (_, bytes) = self.chunk.bytes().split_at(offset);
            offset = self.decode_instr(fmt, offset, bytes)?;
        }
        Ok(())
    }

    fn decode_instr(&self, fmt: &mut Formatter<'_>, offset: usize, instr: &[u8]) -> Result<usize, fmt::Error> {
        write!(fmt, "{:04} ", offset)?;
        
        let offset = match OpCode::from_byte(instr[0]) {
            Some(OpCode::LoadConst) => {
                let cid = instr[1];
                writeln!(fmt, "{:16} {: >4} '{}'", OpCode::LoadConst, cid, self.chunk.lookup_const(cid))?;
                offset + 2
            },
            Some(OpCode::LoadConstWide) => {
                let cid =  ConstID::from_le_bytes(instr[1..3].try_into().unwrap());
                writeln!(fmt, "{:16} {: >4} '{}'", OpCode::LoadConstWide, cid, self.chunk.lookup_const(cid))?;
                offset + 3
            },
            Some(opcode) => {
                writeln!(fmt, "{}", opcode)?;
                offset + 1
            },
            None => {
                writeln!(fmt, "Unknown opcode {:#x}", instr[0])?;
                offset + 1
            }
        };
        Ok(offset)
    }
}

impl fmt::Display for Disassembler<'_> {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        self.decode_chunk(fmt)
    }
}


impl fmt::Display for OpCode {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match *self {
            Self::LoadConst => "OP_LDCONST",
            Self::LoadConstWide => "OP_LDCONST_W",
            Self::Return => "OP_RETURN",
        };
        
        if let Some(width) = fmt.width() {
            write!(fmt, "{:1$}", mnemonic, width)
        } else {
            fmt.write_str(mnemonic)
        }
    }
}

impl fmt::Display for Variant {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => fmt.write_str("nil"),
            Self::EmptyTuple => fmt.write_str("()"),
            Self::Boolean(true) => fmt.write_str("true"),
            Self::Boolean(false) => fmt.write_str("false"),
            Self::Integer(value) => write!(fmt, "{}", value),
            Self::Float(value) => write!(fmt, "{:.6}", value),
            Self::InternStr(sym) => write!(fmt, "$({:?})", sym.symbol()),
        }
    }
}
