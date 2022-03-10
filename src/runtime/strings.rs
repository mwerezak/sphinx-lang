use std::fmt;
use std::rc::Rc;
use std::cell::Ref;
use std::ops::Deref;
use std::hash::{Hash, Hasher, BuildHasher};

mod inline;
use inline::InlineStr;

pub mod string_table;
pub use string_table::StringSymbol;
use string_table::StringTableGuard;



/// Provides support for `StringValue::as_str()` and `StringKey::as_str()`
enum StrRef<'a> {
    Slice(&'a str),
    Ref(Ref<'a, str>),
}

impl Deref for StrRef<'_> {
    type Target = str;
    
    #[inline]
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Slice(string) => string,
            Self::Ref(str_ref) => str_ref,
        }
    }
}


/// Enum over the different string representations.
/// `StringValue` is fairly opaque. That's because if we have an interned string, there aren't many operations
/// we can actually do without having a reference to the `StringTable`. If you do have such a reference, you can
/// either use `StringValue::as_ref()` or do a cheap conversion to `StringKey` which will get you the ability 
/// to hash, compare, or even just check the length of the string.

const INLINE_SIZE: usize = 8;  // TODO figure out size

#[derive(Debug, Clone)]
pub enum StringValue {
    Intern(StringSymbol),
    Inline(InlineStr<INLINE_SIZE>),
    CowRc(Rc<str>),  // uses COW semantics, no need to GC these
}

impl From<StringSymbol> for StringValue {
    #[inline]
    fn from(sym: StringSymbol) -> Self { Self::Intern(sym) }
}

impl From<&str> for StringValue {
    #[inline]
    fn from(string: &str) -> Self {
        // first, try inlining
        InlineStr::try_new(string).map_or_else(
            |string| StringValue::CowRc(Rc::from(string)),
            |inline| StringValue::Inline(inline),
        )
    }
}

impl StringValue {
    pub fn as_str<'a, 's>(&'a self, string_table: &'s StringTableGuard) -> impl Deref<Target=str> + 'a where 's: 'a {
        match self {
            Self::Inline(in_str) => StrRef::Slice(in_str),
            Self::CowRc(rc_str) => StrRef::Slice(rc_str),
            Self::Intern(sym) => StrRef::Ref(string_table.resolve(*sym)),
        }
    }
    
    pub fn into_key<'s>(self, string_table: &'s StringTableGuard) -> StringKey<'s> {
        StringKey::new(self, string_table)
    }
    
    // Performs a comparison if it is possible to do so without a string table
    pub fn try_eq(&self, other: &StringValue) -> Option<bool> {
        let value = match (self, other) {
            (Self::Intern(sym_a), Self::Intern(sym_b)) => sym_a == sym_b,
            
            (Self::Inline(in_a), Self::Inline(in_b)) => in_a.deref() == in_b.deref(),
            (Self::CowRc(rc_a), Self::CowRc(rc_b)) => **rc_a == **rc_b,
            
            (Self::CowRc(rc_str), Self::Inline(in_str)) 
            | (Self::Inline(in_str), Self::CowRc(rc_str)) => **rc_str == *in_str.deref(),
            
            _ => return None,
        };
        Some(value)

    }
}


// For use with VariantKey

#[derive(Debug, Clone)]
pub enum StringKey<'s> {
    Intern(StringSymbol, &'s StringTableGuard),
    Inline(InlineStr<INLINE_SIZE>, u64),
    CowRc(Rc<str>, u64),
}


impl<'s> StringKey<'s> {
    
    /// A reference to the string table is required because we need to compute a hash specifically
    /// using it's hasher in order to produce hashes compatible with interned strings
    #[inline]
    pub fn new(value: StringValue, string_table: &'s StringTableGuard) -> Self {
        match value {
            StringValue::Intern(sym) => Self::Intern(sym, string_table),
            
            // For hash consistency with interned strings, we need to cache a hash produced by the string table
            StringValue::Inline(in_str) => {
                let hash = string_table.make_hash(&in_str);
                Self::Inline(in_str, hash)
            },
            StringValue::CowRc(rc_str) => {
                let hash = string_table.make_hash(&rc_str);
                Self::CowRc(rc_str, hash)
            },
        }
    }
    
    pub fn as_str<'a>(&'a self) -> impl Deref<Target=str> + 'a where 's: 'a {
        match self {
            Self::Inline(in_str, _) => StrRef::Slice(in_str),
            Self::CowRc(rc_str, _) => StrRef::Slice(rc_str),
            Self::Intern(sym, string_table) => StrRef::Ref(string_table.resolve(*sym)),
        }
    }
}

impl Hash for StringKey<'_> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self {
            Self::Inline(_, hash) => hash.hash(state),
            Self::CowRc(_, hash) => hash.hash(state),
            
            Self::Intern(sym, string_table) => {
                let hash = string_table.resolve_hash(*sym).unwrap();
                hash.hash(state);
            },
        }
    }
}

impl Eq for StringKey<'_> { }

impl PartialEq for StringKey<'_> {
    #[inline]
    fn eq(&self, other: &StringKey<'_>) -> bool {
        let self_str = &self.as_str() as &str;
        let other_str = &other.as_str() as &str;
        self_str == other_str
    }
}

// TODO impl PartialOrd, Ord for StringKey
impl PartialOrd for StringKey<'_> {
    fn partial_cmp(&self, other: &StringKey<'_>) -> Option<std::cmp::Ordering> {
        let self_str = &self.as_str() as &str;
        let other_str = &other.as_str() as &str;
        <&str as PartialOrd>::partial_cmp(&self_str, &other_str)
    }
}

impl Ord for StringKey<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let self_str = &self.as_str() as &str;
        let other_str = &other.as_str() as &str;
        <&str as Ord>::cmp(&self_str, &other_str)
    }
}


impl fmt::Display for StringKey<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(&self.as_str())
    }
}

impl From<StringKey<'_>> for StringValue {
    #[inline]
    fn from(strkey: StringKey<'_>) -> Self {
        match strkey {
            StringKey::Intern(sym, _) => Self::Intern(sym),
            StringKey::Inline(in_str, _) => Self::Inline(in_str),
            StringKey::CowRc(rc_str, _) => Self::CowRc(rc_str),
        }
    }
}