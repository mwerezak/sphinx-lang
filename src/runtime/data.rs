use std::hash::BuildHasherDefault;
use rustc_hash::FxHasher;

use string_interner;
use string_interner::backend::Backend;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;

use crate::runtime::Variant;


// Interned Strings

pub type InternHashBuilder = BuildHasherDefault<FxHasher>;
pub type InternSymbol = DefaultSymbol;
pub type InternBackend = DefaultBackend<InternSymbol>;
pub type StringInterner = string_interner::StringInterner<InternBackend, InternHashBuilder>;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InternStr {
    symbol: InternSymbol,
}

impl InternStr {
    pub fn from_str(s: &str, interner: &mut StringInterner) -> Self {
        InternStr { symbol: interner.get_or_intern(s) }
    }
    
    pub fn symbol(&self) -> &InternSymbol { &self.symbol }
}

impl From<InternSymbol> for InternStr {
    fn from(symbol: InternSymbol) -> Self {
        Self { symbol }
    }
}




// wraps an InternStr when hashing so as to allow it to hash and compare consistently with non-interned strings
#[derive(Debug, Clone, Copy)]
struct InternKey<'h> {
    symbol: InternSymbol,
    interner: &'h StringInterner,
}

impl<'h> InternKey<'h> {
    pub fn new(symbol: impl Into<InternSymbol>, interner: &'h StringInterner) -> Self {
        Self { symbol: symbol.into(), interner }
    }
}

// TODO
// impl PartialEq
// impl Eq
// impl Hash

impl From<InternKey<'_>> for InternStr {
    fn from(key: InternKey) -> Self {
        key.symbol.into()
    }
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