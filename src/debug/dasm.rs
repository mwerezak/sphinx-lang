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
        
        let offset = match OpCode::from_byte(instr[0]) {
            Some(OpCode::LoadConst) => {
                let cid = instr[1];
                writeln!(fmt, "{:16} {: >4} '{}'", OpCode::LoadConst, cid, DasmDisplay(self.chunk.lookup_const(cid)))?;
                offset + 2
            },
            Some(OpCode::LoadConstWide) => {
                let cid =  ConstID::from_le_bytes(instr[1..3].try_into().unwrap());
                writeln!(fmt, "{:16} {: >4} '{}'", OpCode::LoadConstWide, cid, DasmDisplay(self.chunk.lookup_const(cid)))?;
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
            Self::LoadConstWide => "OP_LDCONST_16",
            Self::Return => "OP_RETURN",
        };
        
        if let Some(width) = fmt.width() {
            write!(fmt, "{:1$}", mnemonic, width)
        } else {
            fmt.write_str(mnemonic)
        }
    }
}

struct DasmDisplay<'a>(&'a Variant);

impl<'a> From<&'a Variant> for DasmDisplay<'a> {
    fn from(value: &'a Variant) -> Self { Self(value) }
}

impl fmt::Display for DasmDisplay<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = format!("{}", self.0);
        write!(fmt, "{}", utils::trim_str(string.as_str(), 16))
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
    
    pub fn push(&mut self, symbol: DebugSymbol) {
        match self.symbols.last_mut() {
            Some((last, ref mut count)) if *last == symbol && *count < u8::MAX => { 
                *count += 1 
            },
            _ => { self.symbols.push((symbol, 1)) }
        }
    }
}