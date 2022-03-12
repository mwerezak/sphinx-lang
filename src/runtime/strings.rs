use std::fmt;
use std::cmp;
use std::sync::RwLock;
use string_interner::{self, DefaultBackend, DefaultSymbol};
use string_interner::symbol::Symbol;

use crate::runtime::DefaultBuildHasher;


// Interned Strings
pub type InternSymbol = DefaultSymbol;
pub type InternBackend = DefaultBackend<DefaultSymbol>;
pub type StringInterner = string_interner::StringInterner<InternBackend, DefaultBuildHasher>;



#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringSymbol(InternSymbol);

impl StringSymbol {
    fn to_usize(&self) -> usize {
        self.0.to_usize()
    }
    
    pub fn as_str(&self) -> &str {
        // STRING_TABLE.read().unwrap().resolve(self).unwrap()
        unimplemented!()
    }
}

impl From<StringSymbol> for InternSymbol {
    fn from(intern: StringSymbol) -> Self {
        intern.0
    }
}

impl From<InternSymbol> for StringSymbol {
    fn from(symbol: InternSymbol) -> Self {
        Self(symbol)
    }
}

impl From<&str> for StringSymbol {
    fn from(string: &str) -> Self {
        match STRING_TABLE.read().unwrap().get(string) {
            Some(sym) => sym,
            None => STRING_TABLE.write().unwrap().get_or_intern(string),
        }
    }
}

// Lexicographical ordering of strings
impl PartialOrd for StringSymbol {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        <str as PartialOrd>::partial_cmp(self.as_str(), other.as_str())
    }
}

impl Ord for StringSymbol {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        <str as Ord>::cmp(self.as_str(), other.as_str())
    }
}

impl fmt::Display for StringSymbol {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(self.as_str())
    }
}



// Just use a global string table, the alternative (an explosion of pointless lifetime parameters
// for something that is going to live for the entire duration of the program anyways) is much worse


lazy_static! {
    pub static ref STRING_TABLE: RwLock<StringTable> = RwLock::new(StringTable::new());
}


#[derive(Debug)]
pub struct StringTable {
    interner: StringInterner,
}

impl StringTable {
    pub fn new() -> Self {
        StringTable {
            interner: StringInterner::new(),
        }
    }
    
    pub fn get(&self, string: &str) -> Option<StringSymbol> {
        self.interner.get(string).map(|symbol| symbol.into())
    }
    
    pub fn get_or_intern(&mut self, string: &str) -> StringSymbol {
        self.interner.get_or_intern(string).into();
    }
    
    pub fn resolve(&self, sym: impl Into<StringSymbol>) -> Option<&str> {
        self.interner.resolve(sym.into())
    }
}

