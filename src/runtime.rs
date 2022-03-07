mod variant;

pub use variant::{Variant, VariantKey};

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

use strings::{StringInterner, StringKey, StringValue};


// Default Hasher

pub type DefaultHasher = AHasher;
pub type DefaultBuildHasher = ahash::RandomState;
pub type Dictionary<'r> = HashMap<VariantKey<'r>, Variant, DefaultBuildHasher>;
pub type Namespace<'s> = HashMap<StringKey<'s>, Variant, DefaultBuildHasher>;

pub fn new_dictionary<'r>() -> Dictionary<'r> {
    Dictionary::with_hasher(DefaultBuildHasher::default())
}

pub fn new_namespace<'s>() -> Namespace<'s> {
    Namespace::with_hasher(DefaultBuildHasher::default())
}


pub struct Runtime<'r> {
    string_table: StringInterner,
    lexer_factory: LexerBuilder,
    globals: Environment<'r>,
    env_stack: Vec<Environment<'r>>,
}

impl<'r> Runtime<'r> {
    pub fn new() -> Self {
        Runtime {
            string_table: StringInterner::new(),
            lexer_factory: language::create_default_lexer_rules(),
            globals: Environment::new(),
            env_stack: Vec::new(),
        }
    }
    
    // String Table
    
    pub fn string_table(&self) -> &StringInterner { &self.string_table }
    
    pub fn parse_context(&mut self) -> ParseContext {
        ParseContext::new(&self.lexer_factory, &mut self.string_table)
    }
    
    // Environments and Variable Lookup
    
    pub fn global_env(&self) -> &Environment<'r> { &self.globals }
    pub fn global_env_mut(&mut self) -> &mut Environment<'r> { &mut self.globals }
    
    pub fn local_env(&self) -> &Environment<'r> {
        self.env_stack.last().unwrap_or(&self.globals)
    }
    pub fn local_env_mut(&mut self) -> &mut Environment<'r> {
        self.env_stack.last_mut().unwrap_or(&mut self.globals)
    }
    
    pub fn push_env(&mut self) {
        self.env_stack.push(Environment::new());
    }
    
    pub fn pop_env(&mut self) {
        self.env_stack.pop(); // TODO close upvalues
    }
    
    pub fn lookup_value(&self, name: StringValue) -> Option<Variant> {
        for local_env in self.env_stack.iter().rev() {
            let value = local_env.lookup_value(name, &self);
            if value.is_some() {
                return value;
            }
        }
        
        self.globals.lookup_value(name, &self)
    }
}


pub struct Environment<'r> {
    namespace: Namespace<'r>,
}

impl<'r> Environment<'r> {
    pub fn new() -> Environment<'r> {
        Environment { namespace: new_namespace() }
    }
    
    pub fn has_name(&self, name: StringValue, runtime: &Runtime) -> bool {
        let name_key = StringKey::new(name, &runtime.string_table, self.namespace.hasher());
        self.namespace.contains_key(&name_key)
    }
    
    pub fn lookup_value(&self, name: StringValue, runtime: &Runtime) -> Option<Variant> {
        let name_key = StringKey::new(name, &runtime.string_table, self.namespace.hasher());
        self.namespace.get(&name_key).map(|value| *value)
    }
    
    pub fn store_value(&mut self, name: StringValue, value: Variant, runtime: &'r Runtime<'r>) -> Option<Variant> {
        let name_key = StringKey::new(name, &runtime.string_table, self.namespace.hasher());
        self.namespace.insert(name_key, value)
    }
}
