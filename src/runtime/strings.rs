use std::fmt;
use std::rc::Rc;
use std::hash::{Hash, Hasher, BuildHasher};

mod inline;

pub mod string_table;
pub use string_table::StringSymbol;
use string_table::StringTableGuard;


// For use with Variant

/// Enum over the different string representations
#[derive(Debug, Clone)]
pub enum StringValue {
    Intern(StringSymbol),
    CowRc(Rc<str>),  // uses COW semantics, no need to GC these
}

impl From<StringSymbol> for StringValue {
    #[inline]
    fn from(sym: StringSymbol) -> Self { Self::Intern(sym) }
}

impl StringValue {
    // only string values of the same representation can be compared directly
    // otherwise we will need the string table to compare them
    pub fn try_eq(&self, other: &StringValue) -> Option<bool> {
        match (self, other) {
            (Self::Intern(self_sym), Self::Intern(other_sym)) => Some(self_sym == other_sym),
            (Self::CowRc(self_str), Self::CowRc(other_str)) => Some(self_str == other_str),
            _ => None,
        }
    }
    
    pub fn eq(&self, other: &StringValue, string_table: &StringTableGuard) -> bool {
        self.try_eq(other).unwrap_or_else(|| match (self, other) {
            (Self::Intern(sym), Self::CowRc(rc_str)) | (Self::CowRc(rc_str), Self::Intern(sym)) => {
                let intern_str = string_table.resolve(*sym);
                rc_str.as_bytes() == intern_str.as_bytes()
            },
            _ => unreachable!(),
        })
    }
    
    pub fn write_str<'s>(&self, buf: &mut impl fmt::Write, string_table: &'s StringTableGuard) -> fmt::Result {
        match self {
            Self::Intern(sym) => buf.write_str(&string_table.resolve(*sym)),
            Self::CowRc(rc_str) => buf.write_str(rc_str),
        }
    }
}


// For use with VariantKey

#[derive(Debug, Clone)]
pub enum StringKey<'s> {
    Intern { hash: u64, sym: StringSymbol, string_table: &'s StringTableGuard },
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