mod variant;

pub use variant::{Variant, VariantKey, VariantMap};

pub mod strings;
pub mod ops;
pub mod types;
pub mod errors;

mod tests;

use std::collections::HashMap;

use ahash::{self, AHasher};
// use rustc_hash::FxHasher;

use crate::language;
use crate::source::ParseContext;
use crate::lexer::LexerBuilder;
use crate::runtime::strings::StringInterner;


// Default Hasher

pub type DefaultHasher = AHasher;
pub type DefaultBuildHasher = ahash::RandomState;


pub struct Runtime {
    string_table: StringInterner,
    lexer_factory: LexerBuilder,
    // env_stack
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            string_table: StringInterner::new(),
            lexer_factory: language::create_default_lexer_rules(),
        }
    }
    
    pub fn string_table(&self) -> &StringInterner { &self.string_table }
    
    pub fn parse_context(&mut self) -> ParseContext {
        ParseContext::new(&self.lexer_factory, &mut self.string_table)
    }
    
    // TODO return global env, and get local env from global
    pub fn placeholder_env(&mut self) -> Environment<'_> {
        Environment { runtime: self, values: VariantMap::with_hasher(DefaultBuildHasher::default()) }
    }
}


pub struct Environment<'e> {
    runtime: &'e mut Runtime,
    values: VariantMap<'e>,
}

impl<'e> Environment<'e> {
    pub fn runtime(&self) -> &Runtime { self.runtime }
    pub fn runtime_mut(&mut self) -> &mut Runtime { self.runtime }
}
