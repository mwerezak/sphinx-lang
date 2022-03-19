use std::mem;
use std::collections::HashMap;
use string_interner::Symbol as _;
use crate::language::{IntType, FloatType};
use crate::runtime::{Variant, STRING_TABLE};
use crate::runtime::strings::{StringInterner, InternSymbol, StringSymbol};
use crate::runtime::types::function::Signature;
use crate::codegen::errors::{CompileResult, CompileError, ErrorKind};
use crate::debug::DebugSymbol;


pub type ConstID = u16;
pub type ChunkID = u16;
pub type FunctionID = u16;


// Constants

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constant {
    Integer(IntType),
    Float([u8; mem::size_of::<FloatType>()]),
    String(InternSymbol),
    Function(ChunkID, FunctionID),
}

impl From<IntType> for Constant {
    fn from(value: IntType) -> Self { Self::Integer(value) }
}

impl From<FloatType> for Constant {
    fn from(value: FloatType) -> Self { Self::Float(value.to_le_bytes()) }
}

impl From<InternSymbol> for Constant {
    fn from(value: InternSymbol) -> Self { Self::String(value) }
}



/// A buffer used by ChunkBuilder
#[derive(Default)]
pub struct ChunkBuf {
    bytes: Vec<u8>,
    symbols: ChunkSymbolBuf,
}

impl ChunkBuf {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            symbols: ChunkSymbolBuf::new(),
        }
    }
    
    // Bytes
    
    pub fn len(&self) -> usize {
        self.bytes.len()
    }
    
    pub fn as_slice(&self) -> &[u8] {
        self.bytes.as_slice()
    }
    
    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        self.bytes.as_mut_slice()
    }
    
    // using Into<u8> so that OpCodes can be accepted without extra fuss
    pub fn push_byte(&mut self, byte: impl Into<u8>) {
        self.bytes.push(byte.into());
    }
    
    pub fn extend_bytes(&mut self, bytes: &[u8]) {
        self.bytes.extend(bytes);
    }
    
    pub fn patch_bytes(&mut self, offset: usize, patch: &[u8]) {
        let patch_range = offset..(offset + patch.len());
        let target = &mut self.bytes[patch_range];
        target.copy_from_slice(patch);
    }
    
    /// anything previously inside the patch is overwritten
    pub fn resize_patch(&mut self, offset: usize, from_len: usize, to_len: usize) {
        let patch_range = offset..(offset + from_len);
        let patch = std::iter::repeat(u8::default()).take(to_len);
        self.bytes.splice(patch_range, patch);
    }
    
    // Symbols
    
    pub fn push_symbol(&mut self, symbol: DebugSymbol) {
        self.symbols.push(symbol)
    }
    
}


pub struct ChunkBuilder {
    chunks: Vec<ChunkBuf>,
    consts: Vec<Constant>,
    functions: Vec<Signature>,
    dedup: HashMap<Constant, ConstID>,
    strings: StringInterner,
}

impl ChunkBuilder {
    pub fn new() -> Self {
        Self {
            consts: Vec::new(),
            functions: Vec::new(),
            chunks: vec![ ChunkBuf::new() ],
            dedup: HashMap::new(),
            strings: StringInterner::new(),
        }
    }
    
    pub fn with_strings(strings: StringInterner) -> Self {
        Self {
            chunks: vec![ ChunkBuf::new() ],
            functions: Vec::new(),
            consts: Vec::new(),
            dedup: HashMap::new(),
            strings,
        }
    }
    
    // Bytecode
    
    pub fn new_chunk(&mut self) -> CompileResult<ChunkID> {
        let chunk_id = ChunkID::try_from(self.chunks.len())
            .map_err(|_| CompileError::from(ErrorKind::ChunkCountLimit))?;
        
        self.chunks.push(ChunkBuf::new());
        Ok(chunk_id)
    }

    pub fn chunk(&self, chunk_id: ChunkID) -> &ChunkBuf { 
        &self.chunks[usize::from(chunk_id)]
    }
    
    pub fn chunk_mut(&mut self, chunk_id: ChunkID) -> &mut ChunkBuf { 
        &mut self.chunks[usize::from(chunk_id)]
    }
    
    // Constants
    
    pub fn get_or_insert_const(&mut self, value: Constant) -> CompileResult<ConstID> {
        if let Constant::String(symbol) = value {
            debug_assert!(self.strings.resolve(symbol).is_some());
        }
        
        if let Some(cid) = self.dedup.get(&value) {
            Ok(*cid)
        } else {
            let cid = ConstID::try_from(self.consts.len())
                .map_err(|_| CompileError::from(ErrorKind::ConstPoolLimit))?;
            self.consts.push(value);
            self.dedup.insert(value, cid);
            Ok(cid)
        }
    }
    
    pub fn get_or_insert_str(&mut self, string: &str) -> CompileResult<ConstID> {
        let symbol = self.strings.get_or_intern(string);
        self.get_or_insert_const(Constant::String(symbol))
    }
    
    pub fn build(mut self) -> UnloadedProgram {
        self.strings.shrink_to_fit();
        
        let chunks = self.chunks.into_iter()
            .map(|buf| Chunk {
                bytes: buf.bytes.into_boxed_slice(),
                symbols: Some(buf.symbols.build()),
            })
            .collect::<Vec<Chunk>>();
        
        UnloadedProgram {
            consts: self.consts.into_boxed_slice(),
            chunks: chunks.into_boxed_slice(),
            functions: self.functions.into_boxed_slice(),
            strings: self.strings,
        }
    }
}


#[derive(Debug)]
pub struct Chunk {
    bytes: Box<[u8]>,
    symbols: Option<ChunkSymbols>,
}

impl Chunk {
    pub fn bytes(&self) -> &[u8] { &*self.bytes }
    
    pub fn debug_symbols(&self) -> Option<&ChunkSymbols> {
        self.symbols.as_ref()
    }
}


/// A program whose strings have not been yet been loaded into the thread-local string table
#[derive(Debug)]
pub struct UnloadedProgram {
    chunks: Box<[Chunk]>,
    consts: Box<[Constant]>,
    functions: Box<[Signature]>,
    strings: StringInterner,
}

impl UnloadedProgram {
    pub fn chunk(&self, chunk_id: ChunkID) -> &[u8] {
        self.chunks[usize::from(chunk_id)].bytes()
    }
    
    pub fn iter_chunks(&self) -> impl Iterator<Item=&Chunk> {
        self.chunks.iter()
    }
    
    pub fn strings(&self) -> &StringInterner { &self.strings }
    
    pub fn lookup_const(&self, index: impl Into<ConstID>) -> &Constant {
        &self.consts[usize::from(index.into())]
    }
    
    pub fn debug_symbols(&self) -> impl Iterator<Item=&DebugSymbol> {
        self.chunks.iter()
            .filter_map(|chunk| chunk.debug_symbols())
            .flat_map(|symbols| symbols.iter_raw())
    }
}


/// Unlike `UnloadedProgram`, this is not `Send` (mainly because `StringSymbol` is not Send)

#[derive(Debug)]
pub struct Program {
    chunks: Box<[Chunk]>,
    consts: Box<[Constant]>,
    functions: Box<[Signature]>,
    strings: Box<[StringSymbol]>,
}

impl Program {
    #[inline(always)]
    pub fn chunk(&self, chunk_id: ChunkID) -> &[u8] {
        self.chunks[usize::from(chunk_id)].bytes()
    }
    
    pub fn lookup_const(&self, index: impl Into<ConstID>) -> &Constant {
        &self.consts[usize::from(index.into())]
    }
    
    pub fn lookup_value(&self, index: impl Into<ConstID>) -> Variant {
        match self.lookup_const(index) {
            Constant::Integer(value) => Variant::from(*value),
            Constant::Float(bytes) => FloatType::from_le_bytes(*bytes).into(),
            Constant::String(idx) => Variant::from(self.strings[idx.to_usize()]),
            Constant::Function(chunk_id, function_id) => unimplemented!(), // get or create function object
        }
    }
    
    pub fn load(program: UnloadedProgram) -> Self {
        let strings = STRING_TABLE.with(|string_table| {
            let mut interner = string_table.interner_mut();
            
            let mut strings = Vec::with_capacity(program.strings.len());
            for (idx, string) in program.strings.into_iter() {
                debug_assert!(idx.to_usize() == strings.len());
                let symbol = StringSymbol::from(interner.get_or_intern(string));
                strings.push(symbol);
            }
            
            strings.into_boxed_slice()
        });
        
        Self {
            chunks: program.chunks,
            consts: program.consts,
            functions: program.functions,
            strings
        }
    }
    
    // prepares a Chunk for exporting to a file
    pub fn unload(self) -> UnloadedProgram {
        let strings = STRING_TABLE.with(|string_table| {
            let interner = string_table.interner();
            
            let mut strings = StringInterner::with_capacity(self.strings.len());
            for symbol in self.strings.into_iter() {
                let string = interner.resolve((*symbol).into()).unwrap();
                strings.get_or_intern(string);
            }
            strings.shrink_to_fit();
            strings
        });
        
        UnloadedProgram {
            chunks: self.chunks,
            consts: self.consts,
            functions: self.functions,
            strings,
        }
    }
}


// Container for debug symbols generated for bytecode
// Should contain a DebugSymbol for each opcode in the 
// associated Chunk, and in the same order.
#[derive(Debug, Default, Clone)]
pub struct ChunkSymbolBuf {
    symbols: Vec<(DebugSymbol, u8)>,  // run length encoding
}

impl ChunkSymbolBuf {
    pub fn new() -> Self {
        Self {
            symbols: Vec::default(),
        }
    }
    
    pub fn push(&mut self, symbol: DebugSymbol) {
        match self.symbols.last_mut() {
            Some((last, ref mut count)) if *last == symbol && *count < u8::MAX => { 
                *count += 1 
            },
            _ => { self.symbols.push((symbol, 0)) }
        }
    }
    
    pub fn build(self) -> ChunkSymbols {
        ChunkSymbols { symbols: self.symbols.into_boxed_slice() }
    }
}

#[derive(Debug, Clone)]
pub struct ChunkSymbols {
    symbols: Box<[(DebugSymbol, u8)]>,  // run length encoding
}

impl ChunkSymbols {
    pub fn iter(&self) -> impl Iterator<Item=&DebugSymbol> { 
        self.symbols.iter().flat_map(
            |(sym, count)| std::iter::repeat(sym).take(usize::from(*count) + 1)
        )
    }
    
    pub fn iter_raw(&self) -> impl Iterator<Item=&DebugSymbol> {
        self.symbols.iter().map(|(sym, count)| sym)
    }
}