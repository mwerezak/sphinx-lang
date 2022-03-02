use string_interner;
use string_interner::backend::Backend;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;

use crate::runtime::Variant;


// Interned Strings

pub type InternSymbol = DefaultSymbol;
pub type InternBackend = DefaultBackend<InternSymbol>;
pub type StringInterner = string_interner::StringInterner<InternBackend>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct InternStr {
    symbol: InternSymbol,
}

impl InternStr {
    pub fn new(symbol: InternSymbol) -> Self {
        InternStr { symbol }
    }
    
    pub fn from_str(s: &str, interner: &mut StringInterner) -> Self {
        InternStr { symbol: interner.get_or_intern(s) }
    }
    
    pub fn symbol(&self) -> InternSymbol { self.symbol }
}


// Chunks

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