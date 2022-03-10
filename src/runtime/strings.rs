use std::fmt;
use std::cell::{RefCell, Ref, RefMut};
use std::hash::{Hash, Hasher, BuildHasher};

use string_interner;
use string_interner::symbol::Symbol;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;

use crate::runtime::DefaultBuildHasher;


// Interned Strings
pub type InternBackend = DefaultBackend<DefaultSymbol>;
pub type StringInterner = string_interner::StringInterner<InternBackend, DefaultBuildHasher>;


// Typically the string interner needs to be borrowed to resolve an InternSymbol
// On occasion it will need to be borrowed mutably when loading a new module or (sometimes) when creating a new string
// These should never happen at the same time, but the compiler cannot check that, so we need to use RefCell
pub type StringTableCell = RefCell<StringTable>;

#[derive(Debug)]
pub struct StringTable {
    // TODO add RwLock if we ever need this to be Sync
    interner: StringInterner,
    hasher_factory: DefaultBuildHasher,
    hash_cache: Vec<u64>,
}

impl StringTable {
    pub fn new() -> RefCell<Self> {
        let string_table = StringTable {
            interner: StringInterner::new(),
            hasher_factory: DefaultBuildHasher::default(),
            hash_cache: Vec::new(),
        };
        RefCell::new(string_table)
    }
    
    pub fn hasher(&self) -> &impl BuildHasher { return &self.hasher_factory }
    
    fn hash_str(&self, string: &str) -> u64 {
        let mut hasher = self.hasher_factory.build_hasher();
        string.hash(&mut hasher);
        hasher.finish()
    }
    
    pub fn get_or_intern(&mut self, string: &str) -> InternSymbol {
        let symbol = self.interner.get_or_intern(string);
        
        let index = symbol.to_usize();
        if index >= self.hash_cache.len() {
            debug_assert!(index == self.hash_cache.len());
            self.hash_cache.insert(index, self.hash_str(string));
        }
        
        symbol.into()
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

// Enum over the different string representations
#[derive(Debug, Clone, Copy)]
pub enum StringValue {
    Intern(InternSymbol),  // TODO store hash to make conversion to StringKey cheaper
    //Object(GCHandle),
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
            (Self::Intern(self_sym), Self::Intern(other_sym)) 
                => Some(self_sym == other_sym),
            // (Self::Object(..), Self::Object(..))
            //     => unimplemented!(),
            // _ => None,
        }
    }
    
    pub fn write_str<'s>(&self, buf: &mut impl fmt::Write, string_table: &'s StringTableCell) -> fmt::Result {
        match self {
            Self::Intern(sym) => {
                buf.write_str(&string_table.borrow().resolve(*sym).unwrap())
            }
        }
    }
}

// For use with VariantKey

#[derive(Debug, Clone, Copy)]
pub enum StringKey<'s> {
    Intern { hash: u64, sym: InternSymbol, string_table: &'s StringTableCell },
    // StrObject { hash: u64, handle: GCHandle },
}

impl Eq for StringKey<'_> { }

impl<'s> PartialEq for StringKey<'s> {
    #[inline]
    fn eq(&self, other: &StringKey<'s>) -> bool {
        match (self, other) {
            (Self::Intern { sym: self_sym, .. }, Self::Intern { sym: other_sym, .. }) 
                => self_sym == other_sym,
            // (_, _) => self.as_str() == other.as_str(),
        }
    }
}

impl Hash for StringKey<'_> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self {
            Self::Intern { hash, .. } => hash.hash(state),
        }
    }
}

impl<'s> StringKey<'s> {
    pub fn new(value: StringValue, string_table: &'s StringTableCell) -> Self {
        match value {
            StringValue::Intern(sym) => Self::from_intern(sym, string_table),
        }
    }
    
    pub fn from_intern(sym: InternSymbol, string_table: &'s StringTableCell) -> Self {
        let borrowed = string_table.borrow();
        let hash = borrowed.resolve_hash(sym).unwrap();
        
        Self::Intern {
            hash, sym, string_table,
        }
    }
    
    // reference to string table is required because we need to compute a hash specifically using it's hasher
    // in order to produce hashes compatible with interned strings
    // pub fn from_object(str_obj: GCHandle, string_table: &'s StringTableCell) -> Self {
}

impl fmt::Display for StringKey<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Intern { sym, string_table, .. } => {
                fmt.write_str(&string_table.borrow().resolve(*sym).unwrap())
            }
        }
    }
}

impl From<StringKey<'_>> for StringValue {
    fn from(strkey: StringKey<'_>) -> Self {
        match strkey {
            StringKey::Intern { sym, .. } => Self::Intern(sym),
        }
    }
}