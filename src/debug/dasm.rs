use std::fmt;
use std::fmt::Formatter;
use crate::runtime::Variant;
use crate::runtime::strings::StringValue;
use crate::vm::chunk::{Chunk, ConstID};
use crate::vm::opcodes::OpCode;
use crate::source::ModuleSource;
use crate::debug::symbol::DebugSymbol;


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
            offset = self.decode_instr(fmt, &offset, bytes)?;
        }
        Ok(())
    }

    fn decode_instr(&self, fmt: &mut Formatter<'_>, offset: &usize, instr: &[u8]) -> Result<usize, fmt::Error> {
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

impl fmt::Display for Variant {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => fmt.write_str("nil"),
            Self::EmptyTuple => fmt.write_str("()"),
            Self::BoolTrue => fmt.write_str("true"),
            Self::BoolFalse => fmt.write_str("false"),
            Self::Integer(value) => write!(fmt, "{}", value),
            Self::Float(value) => write!(fmt, "{:.6}", value),
            Self::String(strval) => write!(fmt, "{}", strval),
            Self::Tuple(items) => {
                let (last, rest) = items.split_last().unwrap();
                fmt.write_str("(")?;
                for item in rest.iter() {
                    write!(fmt, "{}, ", item)?;
                }
                write!(fmt, "{})", last)
            },
        }
    }
}

impl fmt::Display for StringValue {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Intern(sym) => write!(fmt, "$({:?})", sym),
            Self::Inline(in_str) => write!(fmt, "$\"{:?}\"", in_str),
            Self::CowRc(rc_str) => write!(fmt, "*\"{:?}\"", rc_str),
        }
    }
}


// Container for debug symbols generated for bytecode
// Should contain a DebugSymbol for each opcode in the 
// associated Chunk, and in the same order.
#[derive(Clone)]
pub struct ChunkDebugSymbols {
    source: ModuleSource,
    symbols: Vec<(DebugSymbol, u8)>,  // run length encoding
}

impl ChunkDebugSymbols {
    pub fn new(source: ModuleSource) -> Self {
        ChunkDebugSymbols {
            source, symbols: Vec::new(),
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