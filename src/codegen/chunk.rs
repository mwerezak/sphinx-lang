use std::mem;
use std::str;
use std::ops::Range;
use std::collections::HashMap;
use string_interner::Symbol as _;
use crate::language::{IntType, FloatType};
use crate::runtime::{DefaultBuildHasher, Variant, STRING_TABLE};
use crate::runtime::module::ModuleID;
use crate::runtime::strings::{StringInterner, InternSymbol, StringSymbol};
use crate::runtime::types::function::Signature;
use crate::codegen::errors::{CompileResult, CompileError, ErrorKind};
use crate::debug::DebugSymbol;


// these are limited to u16 right now because they are loaded by opcodes
pub type ConstID = u16;
pub type ChunkID = u16;

pub type StringID = usize;
pub type FunctionID = usize;


// Constants

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constant {
    Integer(IntType),
    Float([u8; mem::size_of::<FloatType>()]),  // we might store redundant floats, that's fine
    String(StringID),
    Function(ChunkID, FunctionID),  // signatures are stored separately to save memory - referenced by "FunctionID"
}

impl From<IntType> for Constant {
    fn from(value: IntType) -> Self { Self::Integer(value) }
}

impl From<FloatType> for Constant {
    fn from(value: FloatType) -> Self { Self::Float(value.to_le_bytes()) }
}

impl From<InternSymbol> for Constant {
    fn from(symbol: InternSymbol) -> Self { Self::String(symbol.to_usize()) }
}


// pub enum ChunkTag {
//     Module,
//     Function,
//     DefaultArg,
// }

#[derive(Default, Debug, Clone)]
pub struct ChunkInfo {
    pub symbol: Option<DebugSymbol>,
}

/// A buffer used by ChunkBuilder
#[derive(Debug)]
pub struct ChunkBuf {
    info: ChunkInfo,
    bytes: Vec<u8>,
}

impl ChunkBuf {
    pub fn new(info: ChunkInfo) -> Self {
        Self {
            info,
            bytes: Vec::new(),
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
}


pub struct ChunkBuilder {
    main: ChunkBuf,
    chunks: Vec<ChunkBuf>,
    consts: Vec<Constant>,
    functions: Vec<Signature>,
    dedup: HashMap<Constant, ConstID, DefaultBuildHasher>,
    strings: StringInterner,
}

impl Default for ChunkBuilder {
    fn default() -> Self {
        Self::with_strings(StringInterner::new())
    }
}

impl ChunkBuilder {
    pub fn new() -> Self {
        Self::default()
    }
    
    pub fn with_strings(strings: StringInterner) -> Self {
        Self {
            main: ChunkBuf::new(ChunkInfo::default()),
            chunks: Vec::new(),
            functions: Vec::new(),
            consts: Vec::new(),
            dedup: HashMap::with_hasher(DefaultBuildHasher::default()),
            strings,
        }
    }
    
    // Bytecode
    
    pub fn new_chunk(&mut self, info: ChunkInfo) -> CompileResult<ChunkID> {
        let chunk_id = ChunkID::try_from(self.chunks.len())
            .map_err(|_| CompileError::from(ErrorKind::ChunkCountLimit))?;
        
        self.chunks.push(ChunkBuf::new(info));
        
        Ok(chunk_id)
    }

    pub fn chunk(&self, chunk_id: Option<ChunkID>) -> &ChunkBuf { 
        if let Some(chunk_id) = chunk_id {
            &self.chunks[usize::from(chunk_id)]
        } else {
            &self.main
        }
    }
    
    pub fn chunk_mut(&mut self, chunk_id: Option<ChunkID>) -> &mut ChunkBuf { 
        if let Some(chunk_id) = chunk_id {
            &mut self.chunks[usize::from(chunk_id)]
        } else {
            &mut self.main
        }
    }
    
    // Constants
    
    pub fn get_or_insert_const(&mut self, value: Constant) -> CompileResult<ConstID> {
        if let Constant::String(index) = value {
            let symbol = InternSymbol::try_from_usize(index);
            debug_assert!(self.strings.resolve(symbol.unwrap()).is_some());
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
        self.get_or_insert_const(Constant::String(symbol.to_usize()))
    }
    
    pub fn push_function(&mut self, signature: Signature) -> FunctionID {
        let function_id = FunctionID::from(self.functions.len());
        self.functions.push(signature);
        function_id
    }
    
    // Output
    
    pub fn build(self) -> UnloadedProgram {
        let mut chunks = Vec::new();
        let mut chunk_index = Vec::with_capacity(self.chunks.len());
        
        for chunk in self.chunks.into_iter() {
            let offset = chunks.len();
            let length = chunk.bytes.len();
            chunks.extend(chunk.bytes);
            
            let index = ChunkIndex {
                offset, length,
                info: chunk.info,
            };
            chunk_index.push(index);
        }
        
        let mut strings = Vec::new();
        let mut string_index = Vec::new();
        string_index.resize_with(self.strings.len(), StringIndex::default);
        
        for (symbol, string) in self.strings.into_iter() {
            let bytes = string.as_bytes();
            let offset = strings.len();
            let length = bytes.len();
            strings.extend(bytes);
            
            let index = StringIndex {
                offset, length
            };
            string_index.insert(symbol.to_usize(), index);
        }
        
        UnloadedProgram {
            main: self.main.bytes.into_boxed_slice(),
            chunks: chunks.into_boxed_slice(),
            chunk_index: chunk_index.into_boxed_slice(),
            strings: strings.into_boxed_slice(),
            string_index: string_index.into_boxed_slice(),
            consts: self.consts.into_boxed_slice(),
            functions: self.functions.into_boxed_slice(),
        }
    }
}


// TODO store all chunk bytes in a single array
// TODO figure out how debug symbols will work, esp. at runtime

#[derive(Debug, Default, Clone)]
pub struct ChunkIndex {
    info: ChunkInfo,
    offset: usize,
    length: usize,
}

impl ChunkIndex {
    pub fn as_range(&self) -> Range<usize> {
        self.offset..(self.offset + self.length)
    }
    
    pub fn info(&self) -> &ChunkInfo {
        &self.info
    }
}

#[derive(Debug, Default, Clone)]
pub struct StringIndex {
    offset: usize,
    length: usize,
}

impl StringIndex {
    pub fn as_range(&self) -> Range<usize> {
        self.offset..(self.offset + self.length)
    }
}

/// A program whose strings have not been yet been loaded into the thread-local string table
/// This means that an `UnloadedProgram` cannot be executed. However, it also means that an
/// `UnloadedProgram` is also self-contained, which is useful for exporting to a file or
/// between threads.
#[derive(Debug, Clone)]
pub struct UnloadedProgram {
    main: Box<[u8]>,
    chunks: Box<[u8]>,
    chunk_index: Box<[ChunkIndex]>,
    strings: Box<[u8]>,
    string_index: Box<[StringIndex]>,
    consts: Box<[Constant]>,
    functions: Box<[Signature]>,
}

impl UnloadedProgram {
    pub fn main(&self) -> &[u8] {
        &self.main
    }
    
    pub fn get_chunk(&self, chunk_id: ChunkID) -> &[u8] {
        let chunk_idx = &self.chunk_index[usize::from(chunk_id)];
        &self.chunks[chunk_idx.as_range()]
    }
    
    pub fn chunk_info(&self, chunk_id: ChunkID) -> &ChunkInfo {
        let chunk_idx = &self.chunk_index[usize::from(chunk_id)];
        chunk_idx.info()
    }
    
    pub fn iter_chunks(&self) -> impl Iterator<Item=(ChunkID, &[u8])> {
        self.chunk_index.iter()
            .map(|index| &self.chunks[index.as_range()])
            .enumerate()
            .map(|(chunk_id, chunk)| (ChunkID::try_from(chunk_id).unwrap(), chunk))
    }
    
    pub fn get_string(&self, string_id: StringID) -> &str {
        let string_idx = &self.string_index[usize::from(string_id)];
        str::from_utf8(&self.strings[string_idx.as_range()]).expect("invalid string")
    }
    
    pub fn iter_strings(&self) -> impl Iterator<Item=(StringID, &str)> {
        self.string_index.iter()
            .map(|index| &self.strings[index.as_range()])
            .map(|slice| str::from_utf8(slice).expect("invalid string"))
            .enumerate()
    }
    
    pub fn lookup_const(&self, index: impl Into<ConstID>) -> &Constant {
        &self.consts[usize::from(index.into())]
    }
}


/// Unlike `UnloadedProgram`, this is not `Send` (mainly because `StringSymbol` is not Send)
#[derive(Debug)]
pub struct ProgramData {
    chunks: Box<[u8]>,
    chunk_index: Box<[ChunkIndex]>,
    strings: Box<[StringSymbol]>,
    consts: Box<[Constant]>,
    functions: Box<[Signature]>,
}

impl ProgramData {

    #[inline(always)]
    pub fn get_chunk(&self, chunk_id: ChunkID) -> &[u8] {
        let index = &self.chunk_index[usize::from(chunk_id)];
        &self.chunks[index.as_range()]
    }
    
    pub fn get_const(&self, index: ConstID) -> &Constant {
        &self.consts[usize::from(index)]
    }
    
    pub fn get_string(&self, index: StringID) -> &StringSymbol {
        &self.strings[usize::from(index)]
    }
    
    pub fn get_signature(&self, index: FunctionID) -> &Signature {
        &self.functions[usize::from(index)]
    }
    
}


#[derive(Debug)]
pub struct Program {
    pub main: Box<[u8]>,
    pub data: ProgramData,
}

impl Program {
    pub fn load(program: UnloadedProgram) -> Self {
        let strings = STRING_TABLE.with(|string_table| {
            let mut string_table = string_table.borrow_mut();
            
            let mut strings = Vec::with_capacity(program.strings.len());
            for (_, string) in program.iter_strings() {
                let symbol = string_table.get_or_intern(string);
                strings.push(symbol);
            }
            
            strings.into_boxed_slice()
        });
        
        Self {
            main: program.main,
            data: ProgramData {
                chunks: program.chunks,
                chunk_index: program.chunk_index,
                consts: program.consts,
                functions: program.functions,
                strings
            },
        }
    }
    
    /// prepares an `UnloadedProgram` for exporting to a file
    pub fn unload(self) -> UnloadedProgram {
        let mut strings = Vec::new();
        let mut string_index = Vec::with_capacity(self.data.strings.len());
        
        STRING_TABLE.with(|string_table| {
            let string_table = string_table.borrow();
            
            for symbol in self.data.strings.iter() {
                let bytes = string_table.resolve(symbol).as_bytes();
                
                let offset = strings.len();
                let length = bytes.len();
                let index = StringIndex {
                    offset, length,
                };
                
                strings.extend(bytes);
                string_index.push(index);
            }
            
        });
        
        UnloadedProgram {
            main: self.main,
            chunks: self.data.chunks,
            chunk_index: self.data.chunk_index,
            strings: strings.into_boxed_slice(),
            string_index: string_index.into_boxed_slice(),
            consts: self.data.consts,
            functions: self.data.functions,
        }
    }
}


