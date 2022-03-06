mod variant;

pub use variant::Variant;

pub mod data;
pub mod ops;
pub mod types;
pub mod errors;

mod tests;

use crate::language;
use crate::lexer::LexerBuilder;
use crate::runtime::data::{StringInterner, InternSymbol};


pub struct Runtime {
    pub string_table: StringInterner,
    pub lexer_factory: LexerBuilder,
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
    
    // TODO return global env, and get local env from global
    pub fn placeholder_env(&mut self) -> Environment<'_> {
        Environment { runtime: self }
    }
}

pub struct Environment<'e> {
    runtime: &'e mut Runtime,
}

impl<'e> Environment<'e> {
    pub fn runtime(&self) -> &Runtime { self.runtime }
    pub fn runtime_mut(&mut self) -> &mut Runtime { self.runtime }
}
