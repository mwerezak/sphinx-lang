use crate::runtime::gc::{GC, GCTrace};

pub mod intern;
pub mod inline;

pub use intern::{StringSymbol, StringInterner, STRING_TABLE};

use inline::InlineStr;

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