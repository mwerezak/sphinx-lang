use std::fmt;
use std::cmp;
use std::cell::{RefCell, Ref};
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use string_interner::{self, DefaultBackend, DefaultSymbol};
use string_interner::symbol::Symbol;

use crate::runtime::DefaultBuildHasher;


thread_local! {
    pub static STRING_TABLE: RefCell<StringTable> = RefCell::new(StringTable::new());
}


// Interned Strings

type PhantomUnsend = PhantomData<*mut ()>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringSymbol(InternSymbol, PhantomUnsend);

// Not Send because we depend on the thread-local string table.
// (We can Send strings with some extra work, just not StringSymbols)
// impl !Send for StringSymbol { }

impl StringSymbol {
    fn to_usize(&self) -> usize {
        self.0.to_usize()
    }
    
    /// Interns a string slice, creating a `StringSymbol`
    pub fn intern(string: &str) -> Self {
        STRING_TABLE.with(|string_table| string_table.borrow_mut().get_or_intern(string))
    }
    
    pub fn concat(&self, other: &StringSymbol) -> StringSymbol {
        STRING_TABLE.with(|string_table| {
            let mut string_table = string_table.borrow_mut();
            
            let mut buf = String::new();
            buf.push_str(string_table.resolve(self));
            buf.push_str(string_table.resolve(other));
            string_table.get_or_intern(buf.as_str())
        })
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

impl fmt::Display for StringSymbol {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        STRING_TABLE.with(|string_table| fmt.write_str(
            string_table.borrow().resolve(self)
        ))
    }
}


pub type InternSymbol = DefaultSymbol;
type InternBackend = DefaultBackend<DefaultSymbol>;

// StringInterner is used for storage of strings in code units during compilation,
// StringTable is used for string symbol lookups at runtime
pub type StringInterner = string_interner::StringInterner<InternBackend, DefaultBuildHasher>;

#[derive(Default, Clone)]
pub struct StringTable {
    interner: StringInterner,
}

impl StringTable {
    pub fn new() -> Self {
        StringTable {
            interner: StringInterner::new(),
        }
    }
    
    pub fn get(&self, string: &str) -> Option<StringSymbol> {
        self.interner.get(string).map(|symbol| symbol.into())
    }
    
    pub fn get_or_intern(&mut self, string: &str) -> StringSymbol {
        self.interner.get_or_intern(string).into()
    }
    
    pub fn resolve(&self, symbol: &StringSymbol) -> &str {
        let symbol = InternSymbol::from(*symbol);
        self.interner.resolve(symbol).expect("invalid symbol")
    }
    
    // pub fn into_iter(&self) -> impl Iterator<Item=(StringSymbol, &str)> {
    //     self.interner.into_iter().map(|(symbol, string)| (symbol.into(), string))
    // }
}

impl<'s> Extend<&'s str> for StringTable {
    fn extend<T>(&mut self, iter: T) where T: IntoIterator<Item=&'s str> {
        for string in iter.into_iter() {
            self.interner.get_or_intern(string);
        }
    }
}
