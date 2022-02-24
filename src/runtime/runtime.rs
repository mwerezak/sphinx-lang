use std::fmt;
use string_interner::{Symbol, StringInterner};
use string_interner::backend::Backend;

use string_interner::DefaultSymbol;
use string_interner::DefaultBackend;

use crate::lexer::{Lexer, LexerBuilder};
use crate::parser::Parser;

pub type StrSymbol = DefaultSymbol;
pub type StrBackend = DefaultBackend<DefaultSymbol>;

// container for data structures necessary for the language runtime
pub struct Runtime {
    interner: StringInterner<StrBackend>,
    lexer_factory: LexerBuilder,
    // globals
    // loaded modules
}

impl Runtime {
    
    pub fn new(lexer_factory: LexerBuilder) -> Self {
        Runtime { 
            lexer_factory, 
            interner: Default::default(),
        }
    }
    
    pub fn create_parser<'m, 'h, S>(&'h mut self, module: &'m str, source: S) -> Parser<'m, 'h, Lexer<S>>
    where S: Iterator<Item=char> {
        let lexer = self.lexer_factory.build(source);
        Parser::new(module, &mut self.interner, lexer)
    }
    
    // Interning Strings
    
    pub fn get_or_intern_str<S>(&mut self, string: S) -> InternStr<StrSymbol> 
    where S: AsRef<str> {
        InternStr::from_str(string.as_ref(), &mut self.interner)
    }
    
    // Attempting to resolve an InternStr that was created with a different 
    // Runtime instance may produce unpredictable results or panic.
    pub fn resolve_str(&self, string: InternStr<StrSymbol>) -> &str {
        self.interner.resolve(string.symbol).unwrap()
    }
}


// Interned strings
#[derive(Debug, Clone, Copy)]
pub struct InternStr<S=DefaultSymbol> where S: Symbol {
    symbol: S,
}

impl<S> InternStr<S> where S: Symbol {
    pub fn from_str<B>(s: &str, interner: &mut StringInterner<B>) -> Self
    where B: Backend<Symbol=S> {
        InternStr { symbol: interner.get_or_intern(s) }
    }
}

impl fmt::Display for InternStr {
    fn fmt(&self, _fmt: &mut fmt::Formatter) -> fmt::Result {
        // fmt.write_str(self.s)
        unimplemented!()
    }
}
