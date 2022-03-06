use std::hash::{BuildHasherDefault, BuildHasher, Hasher, Hash};
use std::collections::HashMap;

use ahash::{self, AHasher};
// use rustc_hash::FxHasher;

use string_interner;
use string_interner::symbol::Symbol;
use string_interner::backend::Backend;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;

use crate::runtime::Variant;


pub type DefaultHasher = AHasher;
pub type DefaultBuildHasher = ahash::RandomState;

// Interned Strings
pub type InternBackend = DefaultBackend<DefaultSymbol>;
pub type StringInterner = string_interner::StringInterner<InternBackend, DefaultBuildHasher>;


// TODO rename to InternSymbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InternStr(DefaultSymbol);

impl InternStr {
    pub fn from_str(s: &str, interner: &mut StringInterner) -> Self {
        InternStr(interner.get_or_intern(s))
    }
    
    pub fn to_usize(&self) -> usize {
        self.0.to_usize()
    }
}

impl From<InternStr> for DefaultSymbol {
    fn from(intern: InternStr) -> Self {
        intern.0
    }
}

impl From<DefaultSymbol> for InternStr {
    fn from(symbol: DefaultSymbol) -> Self {
        Self(symbol)
    }
}
