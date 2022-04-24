use core::fmt;
use core::cmp;
use core::cell::RefCell;
use core::marker::PhantomData;
use core::hash::{Hash, Hasher, BuildHasher};
use string_interner::{self, DefaultBackend};
use string_interner::symbol::Symbol;

use crate::language::InternSymbol;
use crate::runtime::DefaultBuildHasher;


thread_local! {
    pub static STRING_TABLE: RefCell<StringTable> = RefCell::new(StringTable::new());
}

// Helper macro for static interned strings
#[macro_export]
macro_rules! static_symbol {
    ($str:expr) => {
        {
            type StringSymbol = crate::runtime::strings::StringSymbol;
            thread_local! {
                static SYMBOL: StringSymbol = StringSymbol::from($str);
            }
            SYMBOL.with(|symbol| *symbol)
        }
    };
}

pub use static_symbol;

// Interned Strings

type PhantomUnsend = PhantomData<*mut ()>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringSymbol(InternSymbol, PhantomUnsend);

// Not Send because we depend on the thread-local string table.
// (We can Send strings with some extra work, just not StringSymbols)
// impl !Send for StringSymbol { }

impl StringSymbol {
    fn as_usize(&self) -> usize {
        self.0.to_usize()
    }
    
    /// Interns a string slice, creating a `StringSymbol`
    pub fn intern(string: &str) -> Self {
        STRING_TABLE.with(|string_table| string_table.borrow_mut().get_or_intern(string))
    }
    
    pub fn write(&self, buf: &mut impl fmt::Write) -> fmt::Result {
        STRING_TABLE.with(|string_table| buf.write_str(
            string_table.borrow().resolve(self)
        ))
    }
}


// not implementing Deref for StringSymbol because I don't want to hide the cost of acquiring a read lock

impl From<StringSymbol> for InternSymbol {
    fn from(intern: StringSymbol) -> Self {
        intern.0
    }
}

impl From<InternSymbol> for StringSymbol {
    fn from(symbol: InternSymbol) -> Self {
        Self(symbol, PhantomData)
    }
}

impl From<&str> for StringSymbol {
    fn from(string: &str) -> Self {
        Self::intern(string)
    }
}

// Lexicographical ordering of strings
impl PartialOrd for StringSymbol {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        STRING_TABLE.with(|string_table| {
            let string_table = string_table.borrow();
            
            <str as PartialOrd>::partial_cmp(
                string_table.resolve(self),
                string_table.resolve(other),
            )
        })

    }
}

impl Ord for StringSymbol {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        STRING_TABLE.with(|string_table| {
            let string_table = string_table.borrow();
            
            <str as Ord>::cmp(
                string_table.resolve(self),
                string_table.resolve(other),
            )
        })
    }
}

impl fmt::Debug for StringSymbol {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "StringSymbol({})", self.as_usize())
    }
}

impl fmt::Display for StringSymbol {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(fmt)
    }
}


type InternBackend = DefaultBackend<InternSymbol>;

// StringInterner is used for storage of strings in code units during compilation,
// StringTable is used for string symbol lookups at runtime
pub type StringInterner = string_interner::StringInterner<InternBackend, DefaultBuildHasher>;

pub type StringBuildHasher = DefaultBuildHasher;

#[derive(Clone)]
pub struct StringTable {
    interner: StringInterner,
    hasher_factory: StringBuildHasher,
    hashes: Vec<u64>,  // hash cache
}

impl Default for StringTable {
    fn default() -> Self { Self::new() }
}

impl StringTable {
    pub fn new() -> Self {
        StringTable {
            interner: StringInterner::new(),
            hasher_factory: StringBuildHasher::default(),
            hashes: Vec::new(),
        }
    }
    
    pub fn hasher(&self) -> &impl BuildHasher {
        &self.hasher_factory
    }
    
    pub fn hash_str(&self, string: &str) -> u64 {
        let mut state = self.hasher_factory.build_hasher();
        string.hash(&mut state);
        state.finish()
    }
    
    pub fn get(&self, string: &str) -> Option<StringSymbol> {
        self.interner.get(string).map(|symbol| symbol.into())
    }
    
    pub fn get_or_intern(&mut self, string: &str) -> StringSymbol {
        let symbol = self.interner.get_or_intern(string);
        
        // this works because symbols are generated with contiguous values
        debug_assert!(symbol.to_usize() <= self.hashes.len());
        if symbol.to_usize() == self.hashes.len() {
            self.hashes.push(self.hash_str(string))
        }
        
        symbol.into()
    }
    
    pub fn resolve(&self, symbol: &StringSymbol) -> &str {
        let symbol = InternSymbol::from(*symbol);
        self.interner.resolve(symbol).expect("invalid symbol")
    }
    
    pub fn lookup_hash(&self, symbol: &StringSymbol) -> u64 {
        *self.hashes.get(symbol.as_usize()).expect("invalid symbol")
    }
    
    // pub fn into_iter(&self) -> impl Iterator<Item=(StringSymbol, &str)> {
    //     self.interner.into_iter().map(|(symbol, string)| (symbol.into(), string))
    // }
    
}

impl<'s> Extend<&'s str> for StringTable {
    fn extend<T>(&mut self, iter: T) where T: IntoIterator<Item=&'s str> {
        for string in iter.into_iter() {
            self.get_or_intern(string);
        }
    }
}
