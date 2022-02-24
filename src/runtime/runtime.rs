use string_interner::StringInterner;

use crate::lexer::{Lexer, LexerBuilder};
use crate::parser::Parser;
use crate::runtime::data::{InternStr, StrBackend};


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
    
    pub fn create_parser<'m, 'h, S>(&'h mut self, module: &'m Module, source: S) -> Parser<'m, 'h, Lexer<S>>
    where S: Iterator<Item=char> {
        let lexer = self.lexer_factory.build(source);
        Parser::new(module, &mut self.interner, lexer)
    }
    
    // Interning Strings
    
    pub fn get_or_intern_str<S>(&mut self, string: S) -> InternStr where S: AsRef<str> {
        InternStr::from_str(string.as_ref(), &mut self.interner)
    }
    
    // Attempting to resolve an InternStr that was created with a different 
    // Runtime instance may produce unpredictable results or panic.
    pub fn resolve_str(&self, string: InternStr) -> &str {
        self.interner.resolve(string.symbol()).unwrap()
    }
}


// runtime execution context
pub struct RuntimeContext<'r> {
    runtime: &'r Runtime,
    // current local scope
}

impl RuntimeContext<'_> {
    pub fn runtime(&self) -> &Runtime { self.runtime }
}


#[derive(Debug)]
pub struct Module {
    name: String,
}

// Temporary for development
pub fn temp_module(name: &str) -> Module {
    Module { name: name.to_string() }
}