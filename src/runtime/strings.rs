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


/// Enum over the different string representations.
/// `StringValue` is fairly opaque. That's because if we have an interned string, there aren't many operations
/// we can actually do without having a reference to the `StringTable`. If you do have such a reference, you can
/// either use `StringValue::as_ref()` or do a cheap conversion to `StringKey` which will get you the ability 
/// to hash, compare, or even just check the length of the string.

const INLINE_SIZE: usize = 8;  // TODO figure out size

#[derive(Debug, Clone)]
pub enum StringValue {
    Inline(InlineStr<INLINE_SIZE>),
    Intern(StringSymbol),
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
}

// Support for StringValue::as_ref()
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


// For use with VariantKey

#[derive(Debug, Clone)]
pub enum StringKey<'s> {
    Inline(InlineStr<INLINE_SIZE>),
    Intern(StringSymbol, &'s StringTableGuard),
    CowRc(Rc<str>),
}


impl<'s> StringKey<'s> {
    
    /// A reference to the string table is required because we need to compute a hash specifically
    /// using it's hasher in order to produce hashes compatible with interned strings
    #[inline]
    pub fn new(value: StringValue, string_table: &'s StringTableGuard) -> Self {
        match value {
            StringValue::Intern(sym) => Self::Intern(sym, string_table),
            StringValue::Inline(in_str) => Self::Inline(in_str),
            StringValue::CowRc(rc_str) => Self::CowRc(rc_str),
        }
    }
    
    // pub fn as_str(&self, string_table: &StringTableGuard) -> impl Deref<Target=str> {
    //     unimplemented!()
    // }
}

impl Hash for StringKey<'_> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self {
            Self::Inline(in_str) => <str as Hash>::hash(in_str, state),
            Self::CowRc(rc_str) => <str as Hash>::hash(rc_str, state),
            
            Self::Intern(sym, string_table) => {
                let hash = string_table.resolve_hash(*sym).unwrap();
                hash.hash(state);
            },
            
            
        }
    }
}

impl Eq for StringKey<'_> { }

impl<'s> PartialEq for StringKey<'s> {
    #[inline]
    fn eq(&self, other: &StringKey<'s>) -> bool {
        unimplemented!()
        // match (self, other) {
        //     (Self::Intern { sym: self_sym, .. }, Self::Intern { sym: other_sym, .. }) => self_sym == other_sym,
        //     (Self::CowRc { string: self_str, .. }, Self::CowRc { string: other_str, .. }) => self_str == other_str,
            
        //     (Self::Intern { sym, string_table, .. }, Self::CowRc { string, .. })
        //     | (Self::CowRc { string, .. }, Self::Intern { sym, string_table, .. }) => {
        //         let intern_str = string_table.resolve(*sym);
        //         intern_str.as_bytes() == string.as_bytes()
        //     },
        // }
    }
}

// TODO impl PartialOrd, Ord for StringKey

impl fmt::Display for StringKey<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        unimplemented!()
        // match self {
        //     Self::Intern(sym, string_table, ..) => fmt.write_str(&string_table.resolve(*sym)),
        //     Self::CowRc(rc_str) => fmt.write_str(string)
        // }
    }
}

impl From<StringKey<'_>> for StringValue {
    #[inline]
    fn from(strkey: StringKey<'_>) -> Self {
        match strkey {
            StringKey::Intern(sym, _) => Self::Intern(sym),
            StringKey::Inline(in_str) => Self::Inline(in_str),
            StringKey::CowRc(rc_str) => Self::CowRc(rc_str),
        }
    }
}