use std::fmt;
use string_interner::{Symbol, StringInterner};
use string_interner::backend::Backend;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;

use crate::language;


// Fundamental data value type
#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Nil,
    Boolean(bool),
    Integer(language::IntType),
    Float(language::FloatType),
    InternString(InternStr),
    //GCObject(Box<..>),
}



// Interned strings

pub type StrSymbol = DefaultSymbol;
pub type StrBackend = DefaultBackend<StrSymbol>;

#[derive(Debug, Clone, Copy)]
pub struct InternStr {
    symbol: StrSymbol,
}

impl InternStr {
    pub fn from_str(s: &str, interner: &mut StringInterner<StrBackend>) -> Self {
        InternStr { symbol: interner.get_or_intern(s) }
    }
    
    pub fn symbol(&self) -> StrSymbol { self.symbol }
}

impl fmt::Display for InternStr {
    fn fmt(&self, _fmt: &mut fmt::Formatter) -> fmt::Result {
        // fmt.write_str(self.s)
        unimplemented!()
    }
}
