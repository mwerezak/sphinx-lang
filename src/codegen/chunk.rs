use core::str;
use core::ops::Range;
use std::collections::HashMap;
use string_interner::Symbol as _;
use crate::language::InternSymbol;
use crate::runtime::{DefaultBuildHasher, STRING_TABLE};
use crate::runtime::strings::{StringInterner, StringSymbol};
use crate::runtime::function::{Signature, Parameter};
use crate::runtime::errors::ErrorKind;
use crate::codegen::consts::{Constant, ConstID, StringID};
use crate::codegen::funproto::{FunctionProto, UnloadedFunction, UnloadedSignature, UnloadedParam, FunctionID};
use crate::codegen::errors::CompileResult;
use crate::debug::DebugSymbol;



#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Chunk {
    Main,
    Function(FunctionID),
}

impl From<FunctionID> for Chunk {
    fn from(fun_id: FunctionID) -> Self {
        Self::Function(fun_id)
    }
}


#[derive(Debug, Clone)]
pub enum ChunkInfo {
    ModuleMain,
    Function {
        symbol: Option<DebugSymbol>,
    },
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
    
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
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
        let patch = core::iter::repeat(u8::default()).take(to_len);
        self.bytes.splice(patch_range, patch);
    }
}


pub struct ChunkBuilder {
    main: ChunkBuf,
    chunks: Vec<ChunkBuf>,
    consts: Vec<Constant>,
    functions: Vec<Option<UnloadedFunction>>,
    dedup: HashMap<Constant, ConstID, DefaultBuildHasher>,
    strings: StringInterner,
}

impl Default for ChunkBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ChunkBuilder {
    pub fn new() -> Self {
        Self::with_strings(StringInterner::new())
    }
    
    pub fn with_strings(strings: StringInterner) -> Self {
        Self {
            main: ChunkBuf::new(ChunkInfo::ModuleMain),
            chunks: Vec::new(),
            functions: Vec::new(),
            consts: Vec::new(),
            dedup: HashMap::with_hasher(DefaultBuildHasher::default()),
            strings,
        }
    }
    
    // Bytecode
    
    pub fn new_chunk(&mut self, info: ChunkInfo) -> CompileResult<Chunk> {
        let chunk_id = FunctionID::try_from(self.chunks.len())
            .map_err(|_| "function count limit reached")?;
        
        self.chunks.push(ChunkBuf::new(info));
        self.functions.push(None);
        
        Ok(chunk_id.into())
    }

    pub fn chunk(&self, chunk_id: Chunk) -> &ChunkBuf {
        match chunk_id {
            Chunk::Main => &self.main,
            Chunk::Function(id) => &self.chunks[usize::from(id)],
        }
    }
    
    pub fn chunk_mut(&mut self, chunk_id: Chunk) -> &mut ChunkBuf { 
        match chunk_id {
            Chunk::Main => &mut self.main,
            Chunk::Function(id) => &mut self.chunks[usize::from(id)],
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
                .map_err(|_| "constant pool limit reached")?;
            self.consts.push(value);
            self.dedup.insert(value, cid);
            Ok(cid)
        }
    }
    
    pub fn get_or_insert_str(&mut self, string: &str) -> StringID {
        let symbol = self.strings.get_or_intern(string);
        symbol.to_usize()
    }
    
    pub fn get_or_insert_error(&mut self, error: ErrorKind, message: &str) -> CompileResult<ConstID> {
        let message = self.get_or_insert_str(message);
        self.get_or_insert_const(Constant::Error { error, message })
    }
    
    pub fn insert_function(&mut self, fun_proto: UnloadedFunction) {
        let fun_index = usize::from(fun_proto.fun_id);
        while self.functions.len() <= fun_index {
            self.functions.resize(fun_index + 1, None);
        }
        
        self.functions[fun_index].replace(fun_proto);
    }
    
    // Output
    
    pub fn build(self) -> UnloadedProgram {
        let bytes_len = self.chunks.iter().map(|chunk| chunk.bytes.len()).sum();
        let mut chunks = Vec::with_capacity(bytes_len);
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
        
        let bytes_len = self.strings.into_iter().map(|(_, s)| s.len()).sum();
        let mut strings = Vec::with_capacity(bytes_len);
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
        
        // truncate trailing `None` values
        let fun_len = self.functions.iter().enumerate().rev()
            .find_map(|(idx, fun)| fun.as_ref().map(|_| idx + 1))
            .unwrap_or(0);
        
        let functions = self.functions.into_iter().take(fun_len)
            .map(|fun| fun.expect("function ids must be contiguous"))
            .collect::<Vec<UnloadedFunction>>();
        
        UnloadedProgram {
            main: self.main.bytes.into_boxed_slice(),
            chunks: chunks.into_boxed_slice(),
            chunk_index: chunk_index.into_boxed_slice(),
            strings: strings.into_boxed_slice(),
            string_index: string_index.into_boxed_slice(),
            consts: self.consts.into_boxed_slice(),
            functions: functions.into_boxed_slice(),
        }
    }
}


// TODO store all chunk bytes in a single array
// TODO figure out how debug symbols will work, esp. at runtime

#[derive(Debug, Clone)]
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
    functions: Box<[UnloadedFunction]>,
}

impl UnloadedProgram {
    pub fn main(&self) -> &[u8] {
        &self.main
    }
    
    pub fn get_chunk(&self, fun_id: FunctionID) -> &[u8] {
        let chunk_idx = &self.chunk_index[usize::from(fun_id)];
        &self.chunks[chunk_idx.as_range()]
    }
    
    pub fn chunk_info(&self, fun_id: FunctionID) -> &ChunkInfo {
        let chunk_idx = &self.chunk_index[usize::from(fun_id)];
        chunk_idx.info()
    }
    
    pub fn iter_chunks(&self) -> impl Iterator<Item=(Chunk, &[u8])> {
        self.chunk_index.iter()
            .map(|index| &self.chunks[index.as_range()])
            .enumerate()
            .map(|(fun_id, chunk)| (Chunk::Function(fun_id.try_into().unwrap()), chunk))
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
    
    pub fn get_const(&self, index: ConstID) -> &Constant {
        &self.consts[usize::from(index)]
    }
    
    pub fn get_function(&self, index: FunctionID) -> &UnloadedFunction {
        &self.functions[usize::from(index)]
    }
}


/// Unlike `UnloadedProgram`, this is not `Send` (mainly because `StringSymbol` is not Send)
#[derive(Debug)]
pub struct ProgramData {
    chunks: Box<[u8]>,
    chunk_index: Box<[ChunkIndex]>,
    strings: Box<[StringSymbol]>,
    consts: Box<[Constant]>,
    functions: Box<[FunctionProto]>,
}

impl ProgramData {

    #[inline(always)]
    pub fn get_chunk(&self, fun_id: FunctionID) -> &[u8] {
        let index = &self.chunk_index[usize::from(fun_id)];
        &self.chunks[index.as_range()]
    }
    
    pub fn chunk_info(&self, fun_id: FunctionID) -> &ChunkInfo {
        let index = &self.chunk_index[usize::from(fun_id)];
        &index.info
    }
    
    pub fn get_const(&self, index: ConstID) -> &Constant {
        &self.consts[usize::from(index)]
    }
    
    pub fn get_string(&self, index: StringID) -> &StringSymbol {
        &self.strings[usize::from(index)]
    }
    
    pub fn get_function(&self, index: FunctionID) -> &FunctionProto {
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
        
        // Convert strings to StringSymbols
        let strings = STRING_TABLE.with(|string_table| {
            let mut string_table = string_table.borrow_mut();
            
            let mut strings = Vec::with_capacity(program.strings.len());
            for (_, string) in program.iter_strings() {
                let symbol = string_table.get_or_intern(string);
                strings.push(symbol);
            }
            strings
        });
        
        let functions: Vec<FunctionProto> = program.functions.into_vec().into_iter()
            .map(|function| {
                let signature = Self::load_signature(function.signature, &program.consts, &strings);
                FunctionProto::new(function.fun_id, signature, function.upvalues)
            })
            .collect();
        
        Self {
            main: program.main,
            data: ProgramData {
                chunks: program.chunks,
                chunk_index: program.chunk_index,
                consts: program.consts,
                functions: functions.into_boxed_slice(),
                strings: strings.into_boxed_slice(),
            },
        }
    }
    
    fn load_name(const_id: ConstID, consts: &[Constant], strings: &[StringSymbol]) -> StringSymbol {
        let string_id = match consts[usize::from(const_id)] {
            Constant::String(symbol) => symbol,
            _ => panic!("invalid name constant")
        };
        strings[usize::from(string_id)]
    }
    
    fn load_signature(signature: UnloadedSignature, consts: &[Constant], strings: &[StringSymbol]) -> Signature {
        let name = signature.name.map(|const_id| Self::load_name(const_id, consts, strings));
        
        let required = signature.required.into_vec().into_iter()
            .map(|param| Self::load_parameter(param, consts, strings)).collect();
            
        let default = signature.default.into_vec().into_iter()
            .map(|param| Self::load_parameter(param, consts, strings)).collect();
        
        let variadic = signature.variadic
            .map(|param| Self::load_parameter(param, consts, strings));
        
        Signature::new(name, required, default, variadic)
    }
    
    fn load_parameter(param: UnloadedParam, consts: &[Constant], strings: &[StringSymbol]) -> Parameter {
        let name = Self::load_name(param.name, consts, strings);
        Parameter::new(name, param.mode)
    }
}


