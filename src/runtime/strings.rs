use std::fmt;
use std::cmp;
use std::ops::Deref;
use std::hash::{Hash, Hasher};
use crate::runtime::gc::{GC, GCTrace};

pub mod intern;
pub mod inline;

pub use intern::{StringSymbol, StringInterner, STRING_TABLE};

use inline::InlineStr;
use intern::StringTable;


#[derive(Debug, Clone, Copy)]
pub enum StringValue {
    Intern(StringSymbol),
    Inline(InlineStr<14>),
    GC(GC<Box<str>>),
}

unsafe impl GCTrace for Box<str> {
    fn trace(&self) { }
    
    fn size_hint(&self) -> usize {
        self.len()
    }
}

impl StringValue {
    pub fn trace(&self) {
        if let Self::GC(gc_str) = self {
            gc_str.mark_trace()
        }
    }
    
    pub fn write(&self, buf: &mut impl fmt::Write) -> fmt::Result {
        match self {
            Self::Intern(symbol) => symbol.write(buf),
            Self::Inline(inline) => buf.write_str(&*inline),
            Self::GC(gc_str) => buf.write_str(&**gc_str),
        }
    }
    
    fn is_intern(&self) -> bool {
        matches!(self, Self::Intern(..))
    }
    
    fn resolve_str<'s, 'h>(&'s self, string_table: &'h StringTable) -> &'s str where 'h: 's {
        match self {
            Self::Intern(symbol) => string_table.resolve(symbol),
            Self::Inline(inline) => &*inline,
            Self::GC(gc_str) => &**gc_str,
        }
    }
    
    fn try_str(&self) -> Option<&str> {
        match self {
            Self::Inline(inline) => Some(&*inline),
            Self::GC(gc_str) => Some(&**gc_str),
            _ => None,
        }
    }
}

// It's important for strings to hash consistently regardless of representation
// This means that a string "foo" should hash the same whether it is Intern, Inline, or GC
// Unfortunately Rust's hash API makes it VERY hard to have precise control of the hash output
// So we either have to lookup and hash the underlying string data each time (which would kill the 
// performance benefits of string interning) or double-hash.
//
// Double-hashing means that when we intern a string, we precompute a hash. Then, when we want to
// hash an interned string, we lookup the precomputed hash and then feed that to the Hasher.
// When hashing a non-interned string, we compute a hash *using the same hasher* that is used to
// pre-compute interned string hashes and feed that to the Hasher (so the cost of hashing a string 
// is only truly doubled when hashing non-interned strings). This ensures that hashes are consistent.
impl Hash for StringValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        STRING_TABLE.with(|string_table| match self {
            Self::Intern(symbol) => {
                string_table.borrow().lookup_hash(symbol).hash(state);
            }
            Self::Inline(inline) => {
                string_table.borrow().hash_str(&*inline).hash(state);
            }
            Self::GC(gc_str) => {
                string_table.borrow().hash_str(&**gc_str).hash(state);
            }
        })
    }
}

impl PartialEq for StringValue {
    fn eq(&self, other: &StringValue) -> bool {
        match (self, other) {
            // both interned
            (Self::Intern(a), Self::Intern(b)) => a == b,
            
            // one interned
            (Self::Intern(symbol), strval) | (strval, Self::Intern(symbol)) => STRING_TABLE.with(|string_table| {
                string_table.borrow().resolve(symbol) == strval.try_str().unwrap()
            }),
            
            // neither interned
            (a, b) => a.try_str().unwrap() == b.try_str().unwrap()
        }
    }
}

impl Eq for StringValue { }

impl PartialOrd for StringValue {
    fn partial_cmp(&self, other: &StringValue) -> Option<cmp::Ordering> {
        let a = self.try_str();
        let b = other.try_str();
        if a.and(b).is_some() {
            return a.unwrap().partial_cmp(b.unwrap())
        }
        
        STRING_TABLE.with(|string_table| {
            let string_table = string_table.borrow();
            self.resolve_str(&string_table)
                .partial_cmp(other.resolve_str(&string_table))
        })
    }
}

impl Ord for StringValue {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let a = self.try_str();
        let b = other.try_str();
        if a.and(b).is_some() {
            return a.unwrap().cmp(b.unwrap())
        }
        
        STRING_TABLE.with(|string_table| {
            let string_table = string_table.borrow();
            self.resolve_str(&string_table)
                .cmp(other.resolve_str(&string_table))
        })
    }
}


impl fmt::Display for StringValue {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(fmt)
    }
}