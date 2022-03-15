use std::fmt;
use std::fmt::{Write, Formatter};
use std::iter;
use string_interner::Symbol as _;

use crate::language::FloatType;
use crate::codegen::OpCode;
use crate::codegen::chunk::{UnloadedChunk, Constant, ConstID};
use crate::debug::symbol::{DebugSymbol, ResolvedSymbol, ResolvedSymbolTable, SymbolResolutionError};


const PAD_WIDTH: usize = 60;

pub struct Disassembler<'c, 's> {
    chunk: &'c UnloadedChunk,
    symbols: Option<&'s DebugSymbols>,
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
    pub fn new(chunk: &'c UnloadedChunk) -> Self {
        Self { chunk, symbols: None, symbol_table: None }
    }
    
    pub fn with_symbols(mut self, symbols: &'s DebugSymbols) -> Self {
        self.symbols.replace(symbols); self
    }
    
    pub fn with_symbol_table(mut self, symbol_table: &'s ResolvedSymbolTable<'s>) -> Self {
        self.symbol_table.replace(symbol_table); self
    }
    
    fn decode_chunk(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        let mut symbols = self.symbols.map(|symbols| symbols.iter());
        let mut last_symbol = None;
        
        let mut offset = 0;
        while offset < self.chunk.bytes().len() {
            let (_, bytes) = self.chunk.bytes().split_at(offset);
            
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

    fn decode_instr(&self, fmt: &mut Formatter<'_>, offset: &usize, instr: &[u8], symbol: Option<Symbol>) -> Result<usize, fmt::Error> {
        let mut line = String::new();
        
        write!(line, "{:04} ", offset)?;
        
        let opcode = OpCode::from_byte(instr[0]);
        match opcode {
            Some(OpCode::LoadConst) => {
                let cid = instr[1];
                write!(line, "{:16} {: >4}    ", opcode.unwrap(), cid)?;
                self.write_const(&mut line, self.chunk.lookup_const(cid))?;
            },
            Some(OpCode::LoadConst16) => {
                let cid =  ConstID::from_le_bytes(instr[1..=2].try_into().unwrap());
                write!(line, "{:16} {: >4}    ", opcode.unwrap(), cid)?;
                self.write_const(&mut line, self.chunk.lookup_const(cid))?;
            },
            Some(opcode) => {
                write!(line, "{:16}", opcode)?;
            },
            None => {
                write!(line, "Unknown! {:#x}", instr[0])?;
            }
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
                
                let line = symbol.iter_lines().nth(0).unwrap_or("").trim_end();
                if symbol.is_multiline() {
                    write!(fmt, "{}...", line)
                } else {
                    fmt.write_str(line)
                }
            },
            
            Some(Err(error)) => write!(fmt, "   ERROR: {}", error),
            
            None => write!(fmt, "    |"),
        }
    }
    
    fn write_const(&self, fmt: &mut impl fmt::Write, value: &Constant) -> fmt::Result {
        if let Constant::String(symbol) = value {
            if let Some(string) = self.chunk.strings().resolve(*symbol) {
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
        self.decode_chunk(fmt)
    }
}


impl fmt::Display for Constant {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(value) => write!(fmt, "'{}'", value),
            Self::Float(bytes) => write!(fmt, "'{:.6}'", FloatType::from_le_bytes(*bytes)),
            Self::String(symbol) => write!(fmt, "${}", symbol.to_usize() + 1),
        }
    }
}


// Container for debug symbols generated for bytecode
// Should contain a DebugSymbol for each opcode in the 
// associated Chunk, and in the same order.
#[derive(Debug, Default, Clone)]
pub struct DebugSymbols {
    symbols: Vec<(DebugSymbol, u8)>,  // run length encoding
}

impl DebugSymbols {
    pub fn new() -> Self {
        DebugSymbols {
            symbols: Vec::default(),
        }
    }
    
    pub fn iter(&self) -> impl Iterator<Item=&DebugSymbol> { 
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