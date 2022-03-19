use std::fmt;
use std::fmt::{Write, Formatter};
use std::iter;
use string_interner::Symbol as _;

use crate::language::FloatType;
use crate::codegen::OpCode;
use crate::codegen::chunk::{Chunk, UnloadedProgram, Constant, ConstID, ChunkSymbols};
use crate::debug::symbol::{DebugSymbol, ResolvedSymbol, ResolvedSymbolTable, SymbolResolutionError};


const PAD_WIDTH: usize = 60;

pub struct Disassembler<'c, 's> {
    program: &'c UnloadedProgram,
    symbol_table: Option<&'s ResolvedSymbolTable<'s>>,
}

// helper for Disassembler::try_resolve_symbol()
type ResolvedSymbolResult = Result<ResolvedSymbol, SymbolResolutionError>;
enum Symbol<'s> {
    // None means "repeat"
    Unresolved(Option<&'s DebugSymbol>),
    Resolved(Option<&'s ResolvedSymbolResult>),
}

impl<'c, 's> Disassembler<'c, 's> {
    pub fn new(program: &'c UnloadedProgram) -> Self {
        Self { program, symbol_table: None }
    }
    
    pub fn with_symbol_table(mut self, symbol_table: &'s ResolvedSymbolTable<'s>) -> Self {
        self.symbol_table.replace(symbol_table); self
    }
    
    pub fn write_disassembly(&self, fmt: &mut impl Write) -> fmt::Result {
        for (index, chunk) in self.program.iter_chunks().enumerate() {
            writeln!(fmt, "\n\nchunk {}:\n", index)?;
            self.decode_chunk(fmt, chunk)?;
        }
        
        Ok(())
    }
    
    fn decode_chunk(&self, fmt: &mut impl Write, chunk: &Chunk) -> fmt::Result {
        let mut symbols = chunk.debug_symbols().map(|symbols| symbols.iter());
        let mut last_symbol = None;
        
        let mut offset = 0;
        while offset < chunk.bytes().len() {
            let (_, bytes) = chunk.bytes().split_at(offset);
            
            // get the next unresolved symbol if there are any
            let unresolved = symbols.as_mut().map_or(None, |iter| iter.next());
            let symbol = self.try_resolve_symbol(unresolved, last_symbol);
            last_symbol = unresolved;
            
            offset = self.decode_instr(fmt, &offset, bytes, symbol)?;
        }
        Ok(())
    }
    
    // handles all the logic around whether we have a symbol table, if there was a symbol resolution error, repeats...
    fn try_resolve_symbol<'a>(&self, unresolved: Option<&'a DebugSymbol>, last_symbol: Option<&DebugSymbol>) -> Option<Symbol<'a>> where 's: 'a {
        let resolved = unresolved.and_then(|symbol| self.symbol_table.map_or(
            None,
            |symbol_table| symbol_table.get(&symbol)
        ));
        
        let is_repeat = last_symbol.and(unresolved).is_some() && last_symbol.unwrap() == unresolved.unwrap();

        if resolved.is_some() {
            if !is_repeat { Some(Symbol::Resolved(resolved)) }
            else { Some(Symbol::Resolved(None)) }
        } else if unresolved.is_some() {
            if !is_repeat { Some(Symbol::Unresolved(unresolved)) }
            else { Some(Symbol::Unresolved(None)) }
        } else { None }
    }

    fn decode_instr(&self, fmt: &mut impl Write, offset: &usize, instr: &[u8], symbol: Option<Symbol>) -> Result<usize, fmt::Error> {        let mut line = String::new();
        
        write!(line, "{:04X} ", offset)?;
        
        
        let opcode = OpCode::from_byte(instr[0]);
        match opcode {
            Some(opcode) => match opcode {
                
                OpCode::Drop | OpCode::DropLocals => {
                    let len = instr[1];
                    write!(line, "{:16} {: >4}    ", opcode, len)?;
                }
                
                OpCode::LoadConst => {
                    let cid = instr[1];
                    write!(line, "{:16} {: >4}    ", opcode, cid)?;
                    self.write_const(&mut line, self.program.lookup_const(cid))?;
                },
                
                OpCode::LoadConst16 => {
                    let cid =  ConstID::from_le_bytes(instr[1..=2].try_into().unwrap());
                    write!(line, "{:16} {: >4}    ", opcode, cid)?;
                    self.write_const(&mut line, self.program.lookup_const(cid))?;
                },
                
                OpCode::StoreLocal | OpCode::LoadLocal => {
                    let offset = instr[1];
                    write!(line, "{:16} {: >4}    ", opcode, offset)?;
                },
                OpCode::StoreLocal16 | OpCode::LoadLocal16 => {
                    let offset =  u16::from_le_bytes(instr[1..=2].try_into().unwrap());
                    write!(line, "{:16} {: >4}    ", opcode, offset)?;
                },
                
                OpCode::Tuple => {
                    let len = instr[1];
                    write!(line, "{:16} {: >4}    ", opcode, len)?;
                }
                
                OpCode::UInt8 => {
                    let value = Constant::Integer(instr[1].into());
                    write!(line, "{:16}         ", opcode)?;
                    self.write_const(&mut line, &value)?;
                }
                
                OpCode::Int8 => {
                    let value = Constant::Integer(i8::from_le_bytes([instr[1]]).into());
                    write!(line, "{:16}         ", opcode)?;
                    self.write_const(&mut line, &value)?;
                }
                
                OpCode::Float8 => {
                    let value = FloatType::from(i8::from_le_bytes([instr[1]]));
                    let value = Constant::Float(value.to_le_bytes());
                    write!(line, "{:16}         ", opcode)?;
                    self.write_const(&mut line, &value)?;
                }
                
                OpCode::Jump           |
                OpCode::JumpIfFalse    |
                OpCode::JumpIfTrue     |
                OpCode::PopJumpIfFalse |
                OpCode::PopJumpIfTrue  => {
                    let jmp = i16::from_le_bytes(instr[1..=2].try_into().unwrap());
                    let dest = i128::from(jmp) + i128::try_from(offset + opcode.instr_len()).expect("offset too large");
                    write!(line, "{:16} {: >4} -> {:04X}", opcode, jmp, dest)?;
                }
                
                OpCode::LongJump           |
                OpCode::LongJumpIfFalse    |
                OpCode::LongJumpIfTrue     |
                OpCode::PopLongJumpIfFalse |
                OpCode::PopLongJumpIfTrue  => {
                    let jmp = i32::from_le_bytes(instr[1..=4].try_into().unwrap());
                    let dest = i128::from(jmp) + i128::try_from(offset + opcode.instr_len()).expect("offset too large");
                    write!(line, "{:16} {: >4} -> {:04X}", opcode, jmp, dest)?;
                }
                
                opcode => write!(line, "{:16}", opcode)?,
            },
            
            None => write!(line, "Unknown! {:#x}", instr[0])?,
        }
        
        if let Some(symbol) = symbol {
            if line.len() < PAD_WIDTH {
                line.extend(iter::repeat(' ').take(PAD_WIDTH - line.len()))
            }
            match symbol {
                Symbol::Unresolved(symbol) => self.write_unresolved_symbol(&mut line, symbol)?,
                Symbol::Resolved(symbol) => self.write_debug_symbol(&mut line, symbol)?,
            }
        }
        
        writeln!(fmt, "{}", line)?;
        
        Ok(offset + opcode.map_or(1, |op| op.instr_len()))
    }
    
    fn write_unresolved_symbol(&self, fmt: &mut impl fmt::Write, symbol: Option<&DebugSymbol>) -> fmt::Result {
        match symbol {
            Some(symbol) => write!(fmt, "| ${}:{}", symbol.start, symbol.end),
            None => write!(fmt, "|"),  // repeats
        }
        
    }
    
    fn write_debug_symbol(&self, fmt: &mut impl fmt::Write, symbol: Option<&ResolvedSymbolResult>) -> fmt::Result {
        match symbol {
            Some(Ok(symbol)) => {
                write!(fmt, "{: >4}| ", symbol.lineno())?;
                
                let line = symbol.iter_whole_lines().nth(0).unwrap_or("").trim_end();
                if symbol.is_multiline() {
                    let (before, sym_text) = line.split_at(symbol.start());
                    write!(fmt, "{}`{}...`", before, sym_text)
                } else {
                    let (before, rest) = line.split_at(symbol.start());
                    let (sym_text, after) = rest.split_at(symbol.end() - symbol.start());
                    write!(fmt, "{}`{}`{}", before, sym_text, after)
                }
            },
            
            Some(Err(error)) => write!(fmt, "   ERROR: {}", error),
            
            None => write!(fmt, "    |"),
        }
    }
    
    fn write_const(&self, fmt: &mut impl fmt::Write, value: &Constant) -> fmt::Result {
        if let Constant::String(symbol) = value {
            if let Some(string) = self.program.strings().resolve(*symbol) {
                if string.len() > 16 {
                    return write!(fmt, "\"{}...\"", &string[..13]);
                }
                return write!(fmt, "\"{}\"", string);
            }
        }
        
        write!(fmt, "{}", value)
    }
}

impl fmt::Display for Disassembler<'_, '_> {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        self.write_disassembly(fmt)
    }
}


impl fmt::Display for Constant {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(value) => write!(fmt, "'{}'", value),
            Self::Float(bytes) => write!(fmt, "'{:.6}'", FloatType::from_le_bytes(*bytes)),
            Self::String(symbol) => write!(fmt, "${}", symbol.to_usize() + 1),
            Self::Function(_chunk_id, _function_id) => unimplemented!(), // write signature?
        }
    }
}


