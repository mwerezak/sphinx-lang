use core::fmt;
use core::fmt::{Write, Formatter};
use core::iter;
use string_interner::Symbol as _;

use crate::language::FloatType;
use crate::codegen::OpCode;
use crate::codegen::chunk::{UnloadedProgram, Chunk};
use crate::codegen::consts::{Constant, ConstID};
use crate::codegen::funproto::{UnloadedFunction, FunctionID};
use crate::debug::symbol::{DebugSymbol, DebugSymbolTable, ResolvedSymbol, ResolvedSymbolTable, ChunkSymbols};
use crate::debug::symbol::errors::SymbolResolutionError;


const PAD_WIDTH: usize = 60;

pub struct Disassembler<'c, 's> {
    program: &'c UnloadedProgram,
    symbols: Option<&'s ChunkSymbols>,
    symbol_table: Option<&'s ResolvedSymbolTable<'s>>,
}

// helper for Disassembler::try_resolve_symbol()
type ResolvedSymbolResult<'s> = Result<&'s ResolvedSymbol, &'s SymbolResolutionError>;
enum Symbol<'s> {
    // None means "repeat"
    Unresolved(Option<&'s DebugSymbol>),
    Resolved(Option<ResolvedSymbolResult<'s>>),
}

impl<'c, 's> Disassembler<'c, 's> {
    pub fn new(program: &'c UnloadedProgram) -> Self {
        Self { program, symbols: None, symbol_table: None }
    }
    
    pub fn with_symbols(mut self, symbols: &'s ChunkSymbols) -> Self {
        self.symbols.replace(symbols); self
    }
    
    pub fn with_symbol_table(mut self, symbol_table: &'s ResolvedSymbolTable<'s>) -> Self {
        self.symbol_table.replace(symbol_table); self
    }
    
    pub fn write_disassembly(&self, fmt: &mut impl Write) -> fmt::Result {
        writeln!(fmt, "\n\nmain:\n")?;
        let symbols = self.symbols.and_then(|symbols| symbols.get(&Chunk::Main));
        self.decode_chunk(fmt, self.program.main(), symbols)?;
        
        for (chunk_id, chunk) in self.program.iter_chunks() {
            match chunk_id {
                Chunk::Function(fun_id) => {
                    let function = self.program.get_function(fun_id);
                    let name = function.signature.name
                        .and_then(|cid| self.try_get_string(cid));
                    
                    if let Some(name) = name {
                        writeln!(fmt, "\n\nchunk {} ({}):\n", fun_id, name)?;
                    } else {
                        
                        writeln!(fmt, "\n\nchunk {}:\n", fun_id)?;
                    }
                },
                
                _ => {
                    writeln!(fmt, "\n\nchunk:\n")?;
                }
            }
            
            let symbols = self.symbols.and_then(|symbols| symbols.get(&chunk_id));
            self.decode_chunk(fmt, chunk, symbols)?;
        }
        
        Ok(())
    }
    
    fn decode_chunk(&self, fmt: &mut impl Write, chunk: &[u8], symbols: Option<&'s DebugSymbolTable>) -> fmt::Result {
        let mut symbols = symbols.map(|symbols| symbols.iter().peekable());
        let mut last_symbol = None;
        
        let mut offset = 0;
        while offset < chunk.len() {
            let (_, bytes) = chunk.split_at(offset);
            
            // get the next unresolved symbol if there are any
            let unresolved = symbols.as_mut().and_then(|iter| Self::seek_next_symbol(offset, iter));
            let symbol = self.try_resolve_symbol(unresolved, last_symbol);
            last_symbol = unresolved;
            
            offset = self.decode_instr(fmt, &offset, bytes, symbol)?;
        }
        Ok(())
    }
    
    fn seek_next_symbol(offset: usize, symbols: &mut iter::Peekable<impl Iterator<Item=(usize, &'s DebugSymbol)>>) -> Option<&'s DebugSymbol> {
        while matches!(symbols.peek(), Some((next_offset, _)) if *next_offset < offset) {
            symbols.next();
        }
        
        match symbols.next() {
            Some((next_offset, symbol)) if next_offset == offset => Some(symbol),
            _ => None,
        }
    }
    
    // handles all the logic around whether we have a symbol table, if there was a symbol resolution error, repeats...
    fn try_resolve_symbol<'a>(&self, unresolved: Option<&'a DebugSymbol>, last_symbol: Option<&DebugSymbol>) -> Option<Symbol<'a>> where 's: 'a {
        let resolved = unresolved.and_then(|symbol| self.symbol_table.and_then(
            |symbol_table| symbol_table.lookup(symbol)
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
                    write!(line, "{:16} {: >4}", opcode, len)?;
                }
                
                OpCode::LoadConst => {
                    let cid = ConstID::from(instr[1]);
                    write!(line, "{:16} {: >4}    ", opcode, cid)?;
                    self.write_const(&mut line, self.program.get_const(cid))?;
                },
                
                OpCode::LoadConst16 => {
                    let cid =  ConstID::from_le_bytes(instr[1..=2].try_into().unwrap());
                    write!(line, "{:16} {: >4}    ", opcode, cid)?;
                    self.write_const(&mut line, self.program.get_const(cid))?;
                },
                
                OpCode::LoadFunction => {
                    let fun_id = FunctionID::from(instr[1]);
                    write!(line, "{:16} {: >4}    ", opcode, fun_id)?;
                    self.write_function(&mut line, self.program.get_function(fun_id))?;
                },
                
                OpCode::LoadFunction16 => {
                    let fun_id = FunctionID::from_le_bytes(instr[1..=2].try_into().unwrap());
                    write!(line, "{:16} {: >4}    ", opcode, fun_id)?;
                    self.write_function(&mut line, self.program.get_function(fun_id))?;
                },
                
                OpCode::StoreLocal | OpCode::LoadLocal => {
                    let index = instr[1];
                    write!(line, "{:16} {: >4}", opcode, index)?;
                },
                OpCode::StoreLocal16 | OpCode::LoadLocal16 => {
                    let index =  u16::from_le_bytes(instr[1..=2].try_into().unwrap());
                    write!(line, "{:16} {: >4}", opcode, index)?;
                },
                
                OpCode::StoreUpvalue | OpCode::LoadUpvalue => {
                    let index = instr[1];
                    write!(line, "{:16} {: >4}", opcode, index)?;
                }
                OpCode::StoreUpvalue16 | OpCode::LoadUpvalue16 => {
                    let index =  u16::from_le_bytes(instr[1..=2].try_into().unwrap());
                    write!(line, "{:16} {: >4}", opcode, index)?;
                }
                
                OpCode::CloseUpvalue => {
                    let index = instr[1];
                    write!(line, "{:16} {: >4}", opcode, index)?;
                }
                OpCode::CloseUpvalue16 => {
                    let index =  u16::from_le_bytes(instr[1..=2].try_into().unwrap());
                    write!(line, "{:16} {: >4}", opcode, index)?;
                }
                
                OpCode::Tuple => {
                    let len = instr[1];
                    write!(line, "{:16} {: >4}", opcode, len)?;
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
                
                OpCode::Int16 => {
                    let value = Constant::Integer(i16::from_le_bytes([instr[1], instr[2]]).into());
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
                    let relative = i64::from(jmp) + i64::try_from(opcode.instr_len()).unwrap();
                    write!(line, "{:16} {: >4} -> {:04X}", opcode, relative, dest)?;
                }
                
                OpCode::LongJump           |
                OpCode::LongJumpIfFalse    |
                OpCode::LongJumpIfTrue     |
                OpCode::PopLongJumpIfFalse |
                OpCode::PopLongJumpIfTrue  => {
                    let jmp = i32::from_le_bytes(instr[1..=4].try_into().unwrap());
                    let dest = i128::from(jmp) + i128::try_from(offset + opcode.instr_len()).expect("offset too large");
                    let relative = i64::from(jmp) + i64::try_from(opcode.instr_len()).unwrap();
                    write!(line, "{:16} {: >4} -> {:04X}", opcode, relative, dest)?;
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
            Some(symbol) => write!(fmt, "| ${}:{}", symbol.start(), symbol.end()),
            None => write!(fmt, "|"),  // repeats
        }
        
    }
    
    fn write_debug_symbol(&self, fmt: &mut impl fmt::Write, symbol: Option<ResolvedSymbolResult>) -> fmt::Result {
        match symbol {
            Some(Ok(symbol)) => {
                write!(fmt, "{: >4}| ", symbol.lineno())?;
                
                let line = symbol.iter_whole_lines().next().unwrap_or("").trim_end();
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
        if let Constant::String(index) = value {
            let string = self.program.get_string(*index);
            if string.len() > 16 {
                return write!(fmt, "\"{}...\"", &string[..13]);
            }
            return write!(fmt, "\"{}\"", string);
        }
        
        write!(fmt, "{}", value)
    }
    
    fn write_function(&self, fmt: &mut impl fmt::Write, function: &UnloadedFunction) -> fmt::Result {
        if let Some(name) = function.signature.name.and_then(|cid| self.try_get_string(cid)) {
            write!(fmt, "'fun {}()'", name)
        } else {
            write!(fmt, "'fun()'")
        }
    }
    
    fn try_get_string(&self, cid: ConstID) -> Option<&str> {
        match self.program.get_const(cid) {
            Constant::String(index) => Some(self.program.get_string(*index)),
            _ => None,
        }
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
            Self::Integer(value) => write!(fmt, "{}", value),
            Self::Float(bytes) => write!(fmt, "{:.6}", FloatType::from_le_bytes(*bytes)),
            Self::String(symbol) => write!(fmt, "${}", symbol.to_usize() + 1),
            Self::Error { error, .. } => write!(fmt, "{:?}", error),
        }
    }
}

impl fmt::Display for UnloadedFunction {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        write!(fmt, "chunk {}", self.fun_id)
    }
}