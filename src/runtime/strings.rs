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
#[derive(Debug)]
pub struct StringTable {
    // TODO add RwLock if we ever need this to be Sync
    interner: RefCell<StringInterner>,
}

impl StringTable {
    pub fn new() -> Self {
        StringTable {
            interner: RefCell::new(StringInterner::new())
        }
    }
    
    pub fn interner_ref(&self) -> Ref<StringInterner> {
        self.interner.borrow()
    }
    
    pub fn interner_mut(&self) -> RefMut<StringInterner> {
        self.interner.borrow_mut()
    }
}


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

// For use with Variant

// Enum over the different string representations
#[derive(Debug, Clone, Copy)]
pub enum StringValue {
    Intern(InternSymbol),
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
    
    pub fn write_str<'s>(&self, buf: &mut impl fmt::Write, string_table: &'s StringTable) -> fmt::Result {
        match self {
            Self::Intern(sym) => {
                let interner = string_table.interner_ref();
                buf.write_str(interner.resolve((*sym).into()).unwrap())
            }
        }
    }
}

// For use with VariantKey

#[derive(Debug, Clone, Copy)]
pub enum StringKey<'s> {
    Intern { hash: u64, sym: InternSymbol, string_table: &'s StringTable },
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
    pub fn new(value: StringValue, string_table: &'s StringTable, hasher: &impl BuildHasher) -> Self {
        match value {
            StringValue::Intern(sym) => Self::from_intern(sym, string_table, hasher),
        }
    }
    
    pub fn from_intern(sym: InternSymbol, string_table: &'s StringTable, hasher: &impl BuildHasher) -> Self {
        let interner = string_table.interner_ref();
        let string = interner.resolve(sym.into()).unwrap();
        
        let mut hasher = hasher.build_hasher();
        string.hash(&mut hasher);
        let hash = hasher.finish();
        
        Self::Intern {
            hash, sym, string_table,
        }
    }
}

impl fmt::Display for StringKey<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Intern { sym, string_table, .. } => {
                let interner = string_table.interner_ref();
                fmt.write_str(interner.resolve((*sym).into()).unwrap())
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