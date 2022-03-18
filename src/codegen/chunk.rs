use std::mem;
use std::collections::HashMap;
use string_interner::Symbol as _;
use crate::language::{IntType, FloatType};
use crate::runtime::{Variant, STRING_TABLE};
use crate::runtime::strings::{StringInterner, InternSymbol, StringSymbol};
use crate::codegen::errors::{CompileResult, CompileError, ErrorKind};


// Constants

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constant {
    Integer(IntType),
    Float([u8; mem::size_of::<FloatType>()]),
    String(InternSymbol),
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


pub type ConstID = u16;

#[derive(Default)]
pub struct ChunkBuilder {
    bytes: Vec<u8>,
    consts: Vec<Constant>,
    dedup: HashMap<Constant, ConstID>,
    strings: StringInterner,
}

impl ChunkBuilder {
    pub fn with_strings(strings: StringInterner) -> Self {
        Self {
            strings,
            bytes: Vec::new(),
            consts: Vec::new(),
            dedup: HashMap::new(),
        }
    }
    
    // Bytecode
    
    pub fn bytes(&self) -> &[u8] {
        self.bytes.as_slice()
    }
    
    pub fn bytes_mut(&mut self) -> &mut [u8] {
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
    
    pub fn build(mut self) -> UnloadedChunk {
        self.strings.shrink_to_fit();
        
        UnloadedChunk {
            bytes: self.bytes.into_boxed_slice(),
            consts: self.consts.into_boxed_slice(),
            strings: self.strings,
        }
    }
}


/// A chunk whose strings have not been yet been loaded into the thread-local string table

#[derive(Debug)]
pub struct UnloadedChunk {
    bytes: Box<[u8]>,
    consts: Box<[Constant]>,
    strings: StringInterner,
}

impl UnloadedChunk {
    pub fn bytes(&self) -> &[u8] { &*self.bytes }
    
    pub fn strings(&self) -> &StringInterner { &self.strings }
    
    pub fn lookup_const(&self, index: impl Into<ConstID>) -> &Constant {
        &self.consts[usize::from(index.into())]
    }
}


/// Unlike `UnloadedChunk`, this is not `Send` (mainly because `StringSymbol` is not Send)

#[derive(Debug)]
pub struct Chunk {
    bytes: Box<[u8]>,
    consts: Box<[Constant]>,
    strings: Box<[StringSymbol]>,
}

impl Chunk {
    #[inline(always)]
    pub fn bytes(&self) -> &[u8] { &*self.bytes }
    
    pub fn lookup_const(&self, index: impl Into<ConstID>) -> &Constant {
        &self.consts[usize::from(index.into())]
    }
    
    pub fn lookup_value(&self, index: impl Into<ConstID>) -> Variant {
        match self.lookup_const(index) {
            Constant::Integer(value) => Variant::from(*value),
            Constant::Float(bytes) => FloatType::from_le_bytes(*bytes).into(),
            Constant::String(idx) => Variant::from(self.strings[idx.to_usize()]),
        }
    }
    
    pub fn load(chunk: UnloadedChunk) -> Self {
        let strings = STRING_TABLE.with(|string_table| {
            let mut interner = string_table.interner_mut();
            
            let mut strings = Vec::with_capacity(chunk.strings.len());
            for (idx, string) in chunk.strings.into_iter() {
                debug_assert!(idx.to_usize() == strings.len());
                let symbol = StringSymbol::from(interner.get_or_intern(string));
                strings.push(symbol);
            }
            
            strings.into_boxed_slice()
        });
        
        Self {
            bytes: chunk.bytes,
            consts: chunk.consts,
            strings
        }
    }
    
    // prepares a Chunk for exporting to a file
    pub fn unload(self) -> UnloadedChunk {
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
        
        UnloadedChunk {
            bytes: self.bytes,
            consts: self.consts,
            strings,
        }
    }
}