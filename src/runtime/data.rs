use string_interner::{Symbol, StringInterner};
use string_interner::backend::Backend;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;


pub type StrSymbol = DefaultSymbol;
pub type StrBackend = DefaultBackend<StrSymbol>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct InternStr {
    symbol: StrSymbol,
}

impl InternStr {
    pub fn new(symbol: StrSymbol) -> Self {
        InternStr { symbol }
    }
    
    pub fn from_str(s: &str, interner: &mut StringInterner<StrBackend>) -> Self {
        InternStr { symbol: interner.get_or_intern(s) }
    }
    
    pub fn symbol(&self) -> StrSymbol { self.symbol }
}

