use std::fmt;
use std::fmt::Formatter;
use crate::runtime::bytecode::{Chunk, OpCode};
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
            Some(OpCode::Const) => {
                writeln!(fmt, "{} {: >4}", OpCode::Const, instr[1])?;
                offset + 2
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