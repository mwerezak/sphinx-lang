use std::mem;
use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::strings::{InternSymbol, StringInterner, StringTable};
use crate::codegen::errors::{CompileResult, ErrorKind};


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
    strings: StringInterner,
    // dedup: Hash
    
}

impl ChunkBuilder {
    pub fn with_strings(strings: StringInterner) -> Self {
        Self {
            strings,
            bytes: Vec::new(),
            consts: Vec::new(),
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
    
    pub fn lookup_const(&self, index: impl Into<ConstID>) -> &Constant {
        &self.consts[usize::from(index.into())]
    }
    
    pub fn push_const(&mut self, cvalue: Constant) -> CompileResult<ConstID> {
        // TODO dedup
        
        let index = self.consts.len();
        self.consts.push(cvalue);
        
        ConstID::try_from(index)
            .map_err(|_| ErrorKind::ConstPoolLimit.into())
    }
    
    // pub fn strings(&self) -> &StringInterner { &self.strings }
    
    // pub fn lookup_str()
    
    // pub fn push_str()
    
    pub fn build(self) -> UnloadedChunk {
        UnloadedChunk {
            bytes: self.bytes,
            consts: self.consts,
            strings: self.strings,
        }
    }
}


/// A chunk whose strings have not been loaded into the string table
/// This represents code that has been loaded from a file but might be sent
/// to another thread or otherwise not executed right away
pub struct UnloadedChunk {
    bytes: Vec<u8>,
    consts: Vec<Constant>,
    strings: StringInterner,
}

impl UnloadedChunk {
    pub fn bytes(&self) -> &[u8] {
        self.bytes.as_slice()
    }
    
    pub fn lookup_const(&self, index: impl Into<ConstID>) -> &Constant {
        &self.consts[usize::from(index.into())]
    }
    
    // pub fn lookup_value(&self, index: impl Into<ConstID>) -> Variant {

    // }
}


/// Unlike `UnloadedChunk`, this is not `Send` (mainly because `StringSymbol` is not Send)
pub struct Chunk {
    bytes: Vec<u8>,
    consts: Vec<Variant>,
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
    
    pub fn bytes(&self) -> &[u8] {
        self.bytes.as_slice()
    }
    
    pub fn lookup_const(&self, index: impl Into<ConstID>) -> &Variant {
        &self.consts[usize::from(index.into())]
    }
}