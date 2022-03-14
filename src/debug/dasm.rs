use std::fmt;
use std::fmt::{Write, Formatter};
use std::iter;
use crate::utils;
use crate::runtime::Variant;
use crate::codegen::OpCode;
use crate::codegen::chunk::{Chunk, ConstID};
use crate::source::ModuleSource;
use crate::debug::symbol::{DebugSymbol, ResolvedSymbol, ResolvedSymbolTable, SymbolResolutionError};


const PAD_WIDTH: usize = 48;

pub struct Disassembler<'c, 's> {
    chunk: &'c Chunk,
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
    pub fn new(chunk: &'c Chunk) -> Self {
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
        let resolved = unresolved.and(self.symbol_table.map_or(
            None,
            |symbol_table| symbol_table.get(&unresolved.unwrap()))
        );
        
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
                self.write_opcode(&mut line, &opcode.unwrap())?;
                write!(line, " {: >4} ", cid)?;
                self.write_value(&mut line, self.chunk.lookup_const(cid))?;
            },
            Some(OpCode::LoadConst16) => {
                let cid =  ConstID::from_le_bytes(instr[1..=2].try_into().unwrap());
                self.write_opcode(&mut line, &opcode.unwrap())?;
                write!(line, " {: >4} ", cid)?;
                self.write_value(&mut line, self.chunk.lookup_const(cid))?;
            },
            Some(opcode) => {
                self.write_opcode(&mut line, &opcode)?;
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
    
    fn write_opcode(&self, fmt: &mut impl fmt::Write, opcode: &OpCode) -> fmt::Result {
        write!(fmt, "{:16}", opcode)
    }
    
    fn write_value(&self, fmt: &mut impl fmt::Write, value: &Variant) -> fmt::Result {
        if matches!(value, Variant::String(..)) {
            let string = format!("{}", value);
            write!(fmt, "{}", utils::trim_str(string.as_str(), 16))
        } else {
            write!(fmt, "'{}'", value)
        }
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
}

impl fmt::Display for Disassembler<'_, '_> {
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
            
            Self::And => "OP_AND",
            Self::Xor => "OP_XOR",
            Self::Or => "OP_OR",
            Self::Shl => "OP_SHL",
            Self::Shr => "OP_SHR",
            Self::Add => "OP_ADD",
            Self::Sub => "OP_SUB",
            Self::Mul => "OP_MUL",
            Self::Div => "OP_DIV",
            Self::Mod => "OP_MOD",
            Self::EQ => "OP_EQ",
            Self::NE => "OP_NE",
            Self::LT => "OP_LT",
            Self::LE => "OP_LE",
            Self::GE => "OP_GE",
            Self::GT => "OP_GT",
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
#[derive(Clone, Default)]
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