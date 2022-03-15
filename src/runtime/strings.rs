use std::fmt;
use std::cmp;
use std::cell::{RefCell, Ref};
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use string_interner::{self, DefaultBackend, DefaultSymbol};
use string_interner::symbol::Symbol;

use crate::runtime::DefaultBuildHasher;


thread_local! {
    pub static STRING_TABLE: StringTable = StringTable::new();
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
    
    pub fn intern(string: &str) -> Self {
        STRING_TABLE.with(|str_tbl| str_tbl.get_or_intern(string))
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
        STRING_TABLE.with(|str_tbl| {
            <str as PartialOrd>::partial_cmp(
                &str_tbl.resolve(self),
                &str_tbl.resolve(other),
            )
        })

    }
}

impl Ord for StringSymbol {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        STRING_TABLE.with(|str_tbl| {
            <str as Ord>::cmp(
                &str_tbl.resolve(self),
                &str_tbl.resolve(other),
            )
        })
    }
}

impl fmt::Display for StringSymbol {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        STRING_TABLE.with(|str_tbl| fmt.write_str(&str_tbl.resolve(self)))
    }
}


pub type InternSymbol = DefaultSymbol;
type InternBackend = DefaultBackend<DefaultSymbol>;

// StringInterner is used for storage of strings in code units during compilation,
// StringTable is used for string symbol lookups at runtime
pub type StringInterner = string_interner::StringInterner<InternBackend, DefaultBuildHasher>;

#[derive(Clone)]
pub struct StringTable {
    interner: RefCell<StringInterner>,
}

impl StringTable {
    pub fn new() -> Self {
        StringTable {
            interner: RefCell::new(StringInterner::new()),
        }
    }
    
    pub fn get(&self, string: &str) -> Option<StringSymbol> {
        self.interner.borrow_mut().get(string).map(|symbol| symbol.into())
    }
    
    pub fn get_or_intern(&self, string: &str) -> StringSymbol {
        self.interner.borrow_mut().get_or_intern(string).into()
    }
    
    pub fn resolve(&self, symbol: &StringSymbol) -> impl Deref<Target=str> + '_ {
        let symbol = InternSymbol::from(*symbol);
        Ref::map(self.interner.borrow(), |interner| interner.resolve(symbol).unwrap())
    }
    
    // effectively consumes the string table, leaving it empty
    pub fn take_interner(&self) -> StringInterner {
        self.interner.take()
    }

    pub fn interner(&self) -> impl Deref<Target=StringInterner> + '_ {
        self.interner.borrow()
    }
    
    // helpful for interning a bunch of strings without the overhead of repeatedly borrowing
    pub fn interner_mut(&self) -> impl DerefMut<Target=StringInterner> + '_ {
        self.interner.borrow_mut()
    }
}

impl<'s> Extend<&'s str> for StringTable {
    fn extend<T>(&mut self, iter: T) where T: IntoIterator<Item=&'s str> {
        let mut interner = self.interner.borrow_mut();
        for string in iter.into_iter() {
            interner.get_or_intern(string);
        }
    }
}
