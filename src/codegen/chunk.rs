use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
// use crate::runtime::strings::{StringSymbol, StringInterner};
use crate::codegen::errors::{CompileResult, ErrorKind};


// Constants

// #[derive(Debug, Clone)]
// pub enum Constant {
//     Nil,
//     EmptyTuple,
//     BoolTrue,
//     BoolFalse,
//     Integer(IntType),
//     Float([u8; 8]),
//     String(StringSymbol),
// }

// Chunks


pub type ConstID = u16;

#[derive(Default)]
pub struct Chunk {
    bytes: Vec<u8>,
    consts: Vec<Variant>,
    // dedup: Hash
    
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            bytes: Vec::new(),
            consts: Vec::new(),
        }
    }
    
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
    
    pub fn lookup_const(&self, index: impl Into<usize>) -> &Variant {
        &self.consts[index.into()]
    }
    
    pub fn push_const(&mut self, value: Variant) -> CompileResult<ConstID> {
        let index = self.consts.len();
        self.consts.push(value);
        
        ConstID::try_from(index)
            .map_err(|_| ErrorKind::ConstPoolLimit.into())
    }
}


