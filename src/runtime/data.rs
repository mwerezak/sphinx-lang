use string_interner;
use string_interner::backend::Backend;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;


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

