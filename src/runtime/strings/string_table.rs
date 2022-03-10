use std::cell::{RefCell, Ref, RefMut};
use std::hash::{Hash, Hasher, BuildHasher};
use string_interner::{self, DefaultBackend, DefaultSymbol};
use string_interner::symbol::Symbol;

use crate::runtime::DefaultBuildHasher;


// Interned Strings
pub type InternSymbol = DefaultSymbol;
pub type InternBackend = DefaultBackend<DefaultSymbol>;
pub type StringInterner = string_interner::StringInterner<InternBackend, DefaultBuildHasher>;



#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringSymbol(InternSymbol);

impl StringSymbol {
    pub fn to_usize(&self) -> usize {
        self.0.to_usize()
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


// Typically the string interner needs to be borrowed to resolve an StringSymbol
// On occasion it will need to be borrowed mutably when loading a new module or (sometimes) when creating a new string
// These should never happen at the same time, but the compiler cannot check that, so we need to use RefCell

#[derive(Debug)]
pub struct StringTableGuard {
    // TODO add RwLock if we ever need this to be Sync
    internal: RefCell<StringTable>,
}

impl StringTableGuard {
    pub fn new() -> Self {
        let string_table = StringTable {
            hasher_factory: DefaultBuildHasher::default(),
            interner: StringInterner::new(),
            hash_cache: Vec::new(),
        };
        
        StringTableGuard {
            internal: RefCell::new(string_table),
        }
    }
    
    pub fn borrow_mut(&self) -> RefMut<StringTable> {
        self.internal.borrow_mut()
    }
    
    pub fn hasher(&self) -> Ref<impl BuildHasher> {
        let string_table = self.internal.borrow();
        Ref::map(string_table, |string_table| string_table.hasher())
    }
    
    // Option not supported by refcell
    pub fn resolve(&self, sym: StringSymbol) -> Ref<str> {
        let string_table = self.internal.borrow();
        Ref::map(string_table, |string_table| string_table.resolve(sym).unwrap())
    }
    
    pub fn resolve_hash(&self, sym: StringSymbol) -> Option<u64> {
        let string_table = self.internal.borrow();
        string_table.resolve_hash(sym)
    }
}


#[derive(Debug)]
pub struct StringTable {
    hasher_factory: DefaultBuildHasher,
    interner: StringInterner,
    hash_cache: Vec<u64>,
}

impl StringTable {
    pub fn hasher(&self) -> &impl BuildHasher { return &self.hasher_factory }
    
    pub fn get_or_intern(&mut self, string: &str) -> StringSymbol {
        let symbol = self.interner.get_or_intern(string);
        
        let index = symbol.to_usize();
        if index >= self.hash_cache.len() {
            debug_assert!(index == self.hash_cache.len());
            self.hash_cache.insert(index, self.hash_str(string));
        }
        
        symbol.into()
    }
    
    fn hash_str(&self, string: &str) -> u64 {
        let mut hasher = self.hasher_factory.build_hasher();
        string.hash(&mut hasher);
        hasher.finish()
    }
    
    pub fn resolve(&self, sym: StringSymbol) -> Option<&str> {
        self.interner.resolve(sym.into())
    }
    
    pub fn resolve_hash(&self, sym: StringSymbol) -> Option<u64> {
        self.hash_cache.get(DefaultSymbol::from(sym).to_usize()).map(|hash| *hash)
    }
}