use std::fmt;
use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};
use std::hash::{Hash, Hasher, BuildHasher};

use string_interner;
use string_interner::symbol::Symbol;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;

use crate::utils;
use crate::runtime::DefaultBuildHasher;


// Interned Strings
pub type InternBackend = DefaultBackend<DefaultSymbol>;
pub type StringInterner = string_interner::StringInterner<InternBackend, DefaultBuildHasher>;


// Typically the string interner needs to be borrowed to resolve an InternSymbol
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
    pub fn resolve(&self, sym: InternSymbol) -> Ref<str> {
        let string_table = self.internal.borrow();
        Ref::map(string_table, |string_table| string_table.resolve(sym).unwrap())
    }
    
    pub fn resolve_hash(&self, sym: InternSymbol) -> Option<u64> {
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
    
    pub fn get_or_intern(&mut self, string: &str) -> InternSymbol {
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
    
    pub fn resolve(&self, sym: InternSymbol) -> Option<&str> {
        self.interner.resolve(sym.into())
    }
    
    pub fn resolve_hash(&self, sym: InternSymbol) -> Option<u64> {
        self.hash_cache.get(DefaultSymbol::from(sym).to_usize()).map(|hash| *hash)
    }
}


// TODO rename to InternSymbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InternSymbol(DefaultSymbol);

impl InternSymbol {
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

// For use with Variant

/// Enum over the different string representations
#[derive(Debug, Clone)]
pub enum StringValue {
    Intern(InternSymbol),
    CowRc(Rc<str>),  // uses COW semantics, no need to GC these
}

impl From<InternSymbol> for StringValue {
    #[inline]
    fn from(sym: InternSymbol) -> Self { Self::Intern(sym) }
}

impl StringValue {
    // only string values of the same representation can be compared directly
    // otherwise we will need outside help to compare them
    pub fn try_eq(&self, other: &StringValue) -> Option<bool> {
        match (self, other) {
            (Self::Intern(self_sym), Self::Intern(other_sym)) => Some(self_sym == other_sym),
            (Self::CowRc(self_str), Self::CowRc(other_str)) => Some(self_str == other_str),
            _ => None,
        }
    }
    
    pub fn write_str<'s>(&self, buf: &mut impl fmt::Write, string_table: &'s StringTableGuard) -> fmt::Result {
        match self {
            Self::Intern(sym) => buf.write_str(&string_table.resolve(*sym)),
            Self::CowRc(rc_str) => buf.write_str(rc_str),
        }
    }
    
    pub fn as_display<'s>(&'s self, string_table: &'s StringTableGuard) -> impl fmt::Display + 's {
        utils::delegate_fmt(|fmt| self.write_str(fmt, string_table))
    }
}


// For use with VariantKey

#[derive(Debug, Clone)]
pub enum StringKey<'s> {
    Intern { hash: u64, sym: InternSymbol, string_table: &'s StringTableGuard },
    CowRc { hash: u64, string: Rc<str> },
}

impl Eq for StringKey<'_> { }

impl<'s> PartialEq for StringKey<'s> {
    #[inline]
    fn eq(&self, other: &StringKey<'s>) -> bool {
        match (self, other) {
            (Self::Intern { sym: self_sym, .. }, Self::Intern { sym: other_sym, .. }) => self_sym == other_sym,
            (Self::CowRc { string: self_str, .. }, Self::CowRc { string: other_str, .. }) => self_str == other_str,
            
            (Self::Intern { sym, string_table, .. }, Self::CowRc { string, .. })
            | (Self::CowRc { string, .. }, Self::Intern { sym, string_table, .. }) => {
                let intern_str = string_table.resolve(*sym);
                intern_str.as_bytes() == string.as_bytes()
            },
        }
    }
}

impl Hash for StringKey<'_> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self {
            Self::Intern { hash, .. } => hash.hash(state),
            Self::CowRc { hash, .. } => hash.hash(state),
        }
    }
}

impl<'s> StringKey<'s> {
    
    /// A reference to the string table is required because we need to compute a hash specifically
    /// using it's hasher in order to produce hashes compatible with interned strings
    pub fn new(value: StringValue, string_table: &'s StringTableGuard) -> Self {
        match value {
            StringValue::Intern(sym) => {
                let hash = string_table.resolve_hash(sym).unwrap();
                Self::Intern { hash, sym, string_table }
            },
            
            StringValue::CowRc(rc_str) => {
                let mut state = string_table.hasher().build_hasher();
                rc_str.hash(&mut state);
                let hash = state.finish();
                Self::CowRc { hash, string: rc_str }
            }
        }
    }
}

impl fmt::Display for StringKey<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Intern { sym, string_table, .. } => fmt.write_str(&string_table.resolve(*sym)),
            Self::CowRc { string, .. } => fmt.write_str(string)
        }
    }
}

impl From<StringKey<'_>> for StringValue {
    fn from(strkey: StringKey<'_>) -> Self {
        match strkey {
            StringKey::Intern { sym, .. } => Self::Intern(sym),
            StringKey::CowRc { string, .. } => Self::CowRc(string),
        }
    }
}