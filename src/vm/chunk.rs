// Chunks

use crate::runtime::Variant;

pub type ConstID = u16;

#[derive(Default)]
pub struct Chunk {
    bytes: Vec<u8>,
    consts: Vec<Variant>,
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
    
    pub fn push_const(&mut self, value: Variant) -> ConstID {
        let index = self.consts.len();
        self.consts.push(value);
        
        ConstID::try_from(index).expect("constant pool limit reached")
    }
}


