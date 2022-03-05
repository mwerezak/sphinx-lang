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
pub struct InternStr(InternSymbol);

impl InternStr {
    pub fn from_str(s: &str, interner: &mut StringInterner) -> Self {
        InternStr(interner.get_or_intern(s))
    }
}

impl From<InternStr> for InternSymbol {
    fn from(intern: InternStr) -> Self {
        intern.0
    }
}

impl From<InternSymbol> for InternStr {
    fn from(symbol: InternSymbol) -> Self {
        Self(symbol)
    }
}



