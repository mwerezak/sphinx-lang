use core::fmt;
use core::cmp;
use core::hash::{Hash, Hasher};
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::errors::{ExecResult, ErrorKind};

pub mod intern;
pub mod buffer;

pub use intern::{StringSymbol, StringInterner, static_symbol, STRING_TABLE};
pub use buffer::StrBuffer;

use intern::StringTable;

pub type InlineStr = StrBuffer<22>;

#[derive(Debug, Clone, Copy)]
pub enum StringValue {
    Intern(StringSymbol),
    Inline(InlineStr),
    Gc(Gc<str>),
}

unsafe impl GcTrace for str {
    fn trace(&self) { }
    
    fn size_hint(&self) -> usize {
        self.len()
    }
}

impl From<StringSymbol> for StringValue {
    fn from(symbol: StringSymbol) -> Self {
        StringValue::Intern(symbol)
    }
}

impl From<InlineStr> for StringValue {
    fn from(inline: InlineStr) -> Self {
        StringValue::Inline(inline)
    }
}

impl From<Gc<str>> for StringValue {
    fn from(gc_str: Gc<str>) -> Self {
        StringValue::Gc(gc_str)
    }
}

const INTERN_THRESHOLD: usize = 40;

// different constructors for different interning policies
impl StringValue {
    pub fn new_maybe_interned<S>(s: S) -> Self where S: AsRef<str> {
        let string = s.as_ref();
        
        // first, try inlining
        if let Ok(inline) = InlineStr::try_new(string) {
            return Self::Inline(inline);
        }
        
        // always intern short-ish strings
        if string.len() <= INTERN_THRESHOLD {
            return Self::Intern(StringSymbol::intern(string))
        }
        
        Self::Gc(Gc::from_box(string.to_string().into_boxed_str()))
    }
    
    pub fn new_uninterned<S>(s: S) -> Self where S: AsRef<str> {
        let string = s.as_ref();
        
        // first, try inlining
        if let Ok(inline) = InlineStr::try_new(string) {
            return Self::Inline(inline);
        }
        
        Self::Gc(Gc::from_box(string.to_string().into_boxed_str()))
    }
    
    pub fn new_interned<S>(s: S) -> Self where S: AsRef<str> {
        Self::Intern(StringSymbol::from(s.as_ref()))
    }
}

/// evaluates an expression using a StringValue, accessing the STRING_TABLE only if needed
macro_rules! with_strval {
    ($strval:expr, $string:ident => $expr:expr) => {
        match $strval.try_str() {
            Ok($string) => $expr,
            Err(symbol) => STRING_TABLE.with(|string_table| {
                let $string = string_table.borrow().resolve(&symbol);
                $expr
            })
        }
    };
}


impl StringValue {
    pub fn trace(&self) {
        if let Self::Gc(gc_str) = self {
            gc_str.mark_trace()
        }
    }
    
    pub fn write(&self, buf: &mut impl fmt::Write) -> fmt::Result {
        match self {
            Self::Intern(symbol) => symbol.write(buf),
            Self::Inline(inline) => buf.write_str(&*inline),
            Self::Gc(gc_str) => buf.write_str(&**gc_str),
        }
    }
    
    /// Interns the string, producing a StringSymbol
    pub fn as_intern(&self) -> StringSymbol {
        match self {
            Self::Intern(symbol) => *symbol,
            Self::Inline(inline) => StringSymbol::intern(&*inline),
            Self::Gc(gc_str) => StringSymbol::intern(&**gc_str),
        }
    }
    
    /// Interns the string *in place*.
    pub fn make_intern(&mut self) {
        let symbol = self.as_intern();
        *self = Self::Intern(symbol)
    }
    
    fn is_intern(&self) -> bool {
        matches!(self, Self::Intern(..))
    }
    
    fn resolve_str<'s, 'h>(&'s self, string_table: &'h StringTable) -> &'s str where 'h: 's {
        match self {
            Self::Intern(symbol) => string_table.resolve(symbol),
            Self::Inline(inline) => &*inline,
            Self::Gc(gc_str) => &**gc_str,
        }
    }
    
    fn try_str(&self) -> Result<&str, StringSymbol> {
        match self {
            Self::Inline(inline) => Ok(&*inline),
            Self::Gc(gc_str) => Ok(&**gc_str),
            Self::Intern(symbol) => Err(*symbol),
        }
    }
    
    pub fn len(&self) -> usize {
        with_strval!(self, s => s.len())
    }
    
    pub fn char_count(&self) -> usize {
        with_strval!(self, s => s.chars().count())
    }
    
    pub fn concat(&self, other: &StringValue) -> ExecResult<StringValue> {
        // don't allocate when concatenating small strings
        const BUFLEN: usize = 64;
        if self.len() + other.len() <= BUFLEN {
            let mut buf = with_strval!(self, s => StrBuffer::<BUFLEN>::try_new(s).unwrap());
            with_strval!(other, s => buf.try_push_str(s).unwrap());
            
            Ok(StringValue::new_maybe_interned(buf))
        } else {
            let mut buf = String::new();
            with_strval!(self, s => buf.push_str(s));
            with_strval!(other, s => buf.push_str(s));
            
            Ok(StringValue::new_maybe_interned(buf.as_str()))
        }
    }
}

// It's important for strings to hash consistently regardless of representation
// This means that a string "foo" should hash the same whether it is Intern, Inline, or Gc
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
            Self::Intern(symbol) =>
                string_table.borrow().lookup_hash(symbol).hash(state),
            
            Self::Inline(inline) =>
                string_table.borrow().hash_str(&*inline).hash(state),
            
            Self::Gc(gc_str) => 
                string_table.borrow().hash_str(&**gc_str).hash(state),
            
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
        if let (Ok(a), Ok(b)) = (self.try_str(), other.try_str()) {
            return a.partial_cmp(b)
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
        if let (Ok(a), Ok(b)) = (self.try_str(), other.try_str()) {
            return a.cmp(b)
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