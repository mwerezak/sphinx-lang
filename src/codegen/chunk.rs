use std::mem;
use std::collections::HashMap;
use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::strings::{InternSymbol, StringInterner, StringTable};
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
    
    // using Into<u8> so that OpCodes can be accepted without extra fuss
    pub fn push_byte(&mut self, byte: impl Into<u8>) {
        self.bytes.push(byte.into());
    }
    
    pub fn extend_bytes(&mut self, bytes: &[u8]) {
        self.bytes.extend(bytes);
    }
    
    // Constants
    
    pub fn push_const(&mut self, value: Constant) -> CompileResult<ConstID> {
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
    
    pub fn push_str(&mut self, string: &str) -> CompileResult<ConstID> {
        let symbol = self.strings.get_or_intern(string);
        self.push_const(Constant::String(symbol))
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


/// A chunk whose strings have not been loaded into the string table
/// This represents code that has been loaded from a file but might be sent
/// to another thread or otherwise not executed right away
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
pub struct Chunk {
    bytes: Box<[u8]>,
    consts: Box<[Variant]>,
}

impl Chunk {
    pub fn load(chunk: UnloadedChunk) -> Self {
        unimplemented!()
    }
    
    // fn load_const(value: Constant, interner: &mut StringInterner) -> Variant {
    //     match value {
    //         Constant::Integer(value) => Variant::from(value),
    //         Constant::Float(bytes) => FloatType::from_le_bytes(bytes).into(),
    //         Constant::String(symbol) => 
    //     }
    // }
    
    pub fn bytes(&self) -> &[u8] { &*self.bytes }
    
    pub fn lookup_const(&self, index: impl Into<ConstID>) -> &Variant {
        &self.consts[usize::from(index.into())]
    }
}