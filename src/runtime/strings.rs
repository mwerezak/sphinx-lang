use std::fmt;
use std::hash::{Hash, Hasher, BuildHasher};
use ahash::{self, AHasher};
// use rustc_hash::FxHasher;

use string_interner;
use string_interner::symbol::Symbol;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;


//  TODO move elsewhere
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
}

// For use with VariantKey

#[derive(Debug, Clone, Copy)]
pub enum StringKey<'r> {
    Intern { hash: u64, sym: InternSymbol, str_table: &'r StringInterner },
    // StrObject { hash: u64, handle: GCHandle },
}

impl Eq for StringKey<'_> { }

impl<'r> PartialEq for StringKey<'r> {
    #[inline]
    fn eq(&self, other: &StringKey<'r>) -> bool {
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
    pub fn from_intern(sym: InternSymbol, str_table: &'s StringInterner, hasher: &impl BuildHasher) -> Self {
        let string = str_table.resolve(sym.into()).unwrap();
        
        let mut hasher = hasher.build_hasher();
        string.hash(&mut hasher);
        let hash = hasher.finish();
        
        Self::Intern {
            hash, sym, str_table,
        }
    }
    
    pub fn as_str(&self) -> &str {
        match self {
            Self::Intern { sym, str_table, .. } => {
                let sym = (*sym).into();
                str_table.resolve(sym).unwrap()
            }
        }
    }
}

impl fmt::Display for StringKey<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(self.as_str())
    }
}
