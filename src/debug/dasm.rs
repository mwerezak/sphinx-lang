use std::fmt;
use std::fmt::Formatter;
use crate::utils;
use crate::runtime::Variant;
use crate::vm::chunk::{Chunk, ConstID};
use crate::vm::opcodes::OpCode;
use crate::source::ModuleSource;
use crate::debug::symbol::DebugSymbol;


pub struct Disassembler<'c> {
    chunk: &'c Chunk,
    symbols: Option<DebugSymbols>,
}

impl<'c> Disassembler<'c> {
    pub fn new(chunk: &'c Chunk) -> Self {
        Disassembler { chunk, symbols: None }
    }
    
    pub fn with_symbols(mut self, symbols: DebugSymbols) -> Self {
        self.symbols = Some(symbols); self
    }
    
    fn decode_chunk(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        let mut offset = 0;
        while offset < self.chunk.bytes().len() {
            let (_, bytes) = self.chunk.bytes().split_at(offset);
            offset = self.decode_instr(fmt, &offset, bytes)?;
        }
        Ok(())
    }

    fn decode_instr(&self, fmt: &mut Formatter<'_>, offset: &usize, instr: &[u8]) -> Result<usize, fmt::Error> {
        write!(fmt, "{:04} ", offset)?;
        
        let opcode = OpCode::from_byte(instr[0]);
        match opcode {
            Some(OpCode::LoadConst) => {
                let cid = instr[1];
                // writeln!(fmt, "{:16} {: >4} '{}'", OpCode::LoadConst, cid, DasmDisplay(self.chunk.lookup_const(cid)))?;
            },
            Some(OpCode::LoadConst16) => {
                let cid =  ConstID::from_le_bytes(instr[1..3].try_into().unwrap());
                // writeln!(fmt, "{:16} {: >4} '{}'", OpCode::LoadConst16, cid, DasmDisplay(self.chunk.lookup_const(cid)))?;
            },
            Some(opcode) => {
                writeln!(fmt, "{}", opcode)?;
            },
            None => {
                writeln!(fmt, "Unknown opcode {:#x}", instr[0])?;
            }
        }
        Ok(offset + opcode.map_or(1, |op| op.instr_len()))
    }
    
    fn write_value(&self, write: &mut impl fmt::Write, value: &Variant) -> fmt::Result {
        let string = format!("{}", value);
        write!(write, "{}", utils::trim_str(string.as_str(), 16))
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
            Self::Return => "OP_RETURN",
            
            Self::LoadConst => "OP_LDCONST",
            Self::LoadConst16 => "OP_LDCONST_16",
            
            Self::Nil => "OP_NIL",
            Self::Empty => "OP_EMPTY",
            Self::True => "OP_TRUE",
            Self::False => "OP_FALSE",
            
            Self::Neg => "OP_NEG",
            Self::Pos => "OP_POS",
            Self::Inv => "OP_INV",
            Self::Not => "OP_NOT",
        };
        
        if let Some(width) = fmt.width() {
            write!(fmt, "{:1$}", mnemonic, width)
        } else {
            fmt.write_str(mnemonic)
        }
    }
}


// Container for debug symbols generated for bytecode
// Should contain a DebugSymbol for each opcode in the 
// associated Chunk, and in the same order.
#[derive(Clone)]
pub struct DebugSymbols {
    symbols: Vec<(DebugSymbol, u8)>,  // run length encoding
}

impl DebugSymbols {
    pub fn new() -> Self {
        DebugSymbols {
            symbols: Vec::default(),
        }
    }
    
    pub fn symbols(&self) -> impl Iterator<Item=&DebugSymbol> { 
        self.symbols.iter().flat_map(
            |(sym, count)| std::iter::repeat(sym).take(usize::from(*count))
        )
    }
    
    pub fn push(&mut self, symbol: &DebugSymbol) {
        match self.symbols.last_mut() {
            Some((last, ref mut count)) if last == symbol && *count < u8::MAX => { 
                *count += 1 
            },
            _ => { self.symbols.push((*symbol, 1)) }
        }
    }
}