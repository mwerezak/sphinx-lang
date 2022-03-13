use std::fmt;
use std::cmp;
use std::sync::{RwLock, RwLockReadGuard};
use std::ops::Deref;
use string_interner::{self, DefaultBackend, DefaultSymbol};
use string_interner::symbol::Symbol;

use crate::runtime::DefaultBuildHasher;


// Interned Strings

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringSymbol(InternSymbol);

impl StringSymbol {
    fn to_usize(&self) -> usize {
        self.0.to_usize()
    }
    
    /// Returns a Deref that reads the string referenced by this `StringSymbol` from the global string table.
    /// A lock on the string table is held until the value returned from this method is dropped.
    pub fn resolve(&self) -> impl Deref<Target=str> {
        STRING_TABLE.resolve(self)
    }
}

// not implementing Deref for StringSymbol because I don't want to hide the cost of acquiring a read lock

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
        match STRING_TABLE.get(string) {
            Some(sym) => sym,
            None => STRING_TABLE.get_or_intern(string),
        }
    }
}

// Lexicographical ordering of strings
impl PartialOrd for StringSymbol {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        <str as PartialOrd>::partial_cmp(&self.resolve(), &other.resolve())
    }
}

impl Ord for StringSymbol {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        <str as Ord>::cmp(&self.resolve(), &other.resolve())
    }
}

impl fmt::Display for StringSymbol {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(&self.resolve())
    }
}



// I've spent way too much time pondering whether or not making this global is a good idea, and I've
// come to the conclusion that the answer is an overwhelming yes. The string table is unlike almost all 
// other kinds of global data in that the actual associations that it stores don't actually matter, only 
// the content does (you could say its data is content-addressed). Therefore, even two completely unrelated
// programs could share a string table with no problems. The only consideration is thread-safety, which
// is handled by RwLock. In return, we avoid a massive explosion of lifetime parameters and the memory
// footprint of StringSymbol is cut in half since we don't need to store a reference to the string table.

lazy_static! {
    pub static ref STRING_TABLE: StringTable = StringTable::new();
}


type InternSymbol = DefaultSymbol;
type InternBackend = DefaultBackend<DefaultSymbol>;
type StringInterner = string_interner::StringInterner<InternBackend, DefaultBuildHasher>;

#[derive(Debug)]
pub struct StringTable {
    // I expect that writes should be much rarer than reads
    interner: RwLock<StringInterner>,
}

impl<'s> StringTable {
    pub fn new() -> Self {
        StringTable {
            interner: RwLock::new(StringInterner::new()),
        }
    }
    
    pub fn get(&self, string: &str) -> Option<StringSymbol> {
        self.interner.read().unwrap()
            .get(string).map(|symbol| symbol.into())
    }
    
    pub fn get_or_intern(&self, string: &str) -> StringSymbol {
        self.interner.write().unwrap().get_or_intern(string).into()
    }
    
    pub fn resolve(&'s self, symbol: &StringSymbol) -> StrRead<'s> {
        StrRead { 
            symbol: (*symbol).into(), 
            read: self.interner.read().unwrap() 
        }
    }
}

/// Deref for strings resolved by the string table. Holds a read lock on the table until dropped.
pub struct StrRead<'s>{
    symbol: InternSymbol,
    read: RwLockReadGuard<'s, StringInterner>
}

impl<'s> Deref for StrRead<'s> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.read.resolve(self.symbol).unwrap()
    }
}