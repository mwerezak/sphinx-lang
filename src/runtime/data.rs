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
pub struct InternSymbol(DefaultSymbol);

impl InternSymbol {
    pub fn from_str(s: &str, interner: &mut StringInterner) -> Self {
        InternSymbol(interner.get_or_intern(s))
    }
    
    pub fn to_usize(&self) -> usize {
        self.0.to_usize()
    }
}

impl From<InternSymbol> for DefaultSymbol {
    fn from(intern: InternSymbol) -> Self {
        intern.0
    }
}

impl From<DefaultSymbol> for InternSymbol {
    fn from(symbol: DefaultSymbol) -> Self {
        Self(symbol)
    }
}

// Enum over the different string representations

#[derive(Debug, Clone, Copy)]
pub enum StringRepr<'r> {
    InternStr(InternSymbol, &'r StringInterner),
    // StrObject(GCHandle),
}

impl Eq for StringRepr<'_> { }

impl<'r> PartialEq for StringRepr<'r> {
    #[inline]
    fn eq(&self, other: &StringRepr<'r>) -> bool {
        match (self, other) {
            (Self::InternStr(self_sym, _), Self::InternStr(other_sym, _)) => self_sym == other_sym,
            // (_, _) => self.as_str() == other.as_str(),
        }
    }
}

impl StringRepr<'_> {
    pub fn as_str(&self) -> &str {
        match self {
            Self::InternStr(sym, string_table) => {
                let sym = (*sym).into();
                string_table.resolve(sym).unwrap()
            }
        }
    }
}