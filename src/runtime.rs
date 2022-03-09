mod variant;

pub use variant::{Variant, VariantKey};

pub mod strings;
pub mod ops;
pub mod types;
pub mod errors;

mod tests;

use std::cell::RefCell;
use std::collections::HashMap;

use ahash::{self, AHasher};
// use rustc_hash::FxHasher;

use crate::language;
use crate::source::ParseContext;
use crate::lexer::LexerBuilder;

use strings::{StringTable, StringKey, StringValue};


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


#[derive(Debug)]
pub struct Runtime<'r> {
    string_table: &'r StringTable,
    globals: Environment<'r>,
    env_stack: Vec<Environment<'r>>,
}

impl<'r> Runtime<'r> {
    pub fn new(string_table: &'r StringTable) -> Self {
        let globals = Environment { 
            string_table, namespace: new_namespace(),
        };
        
        Runtime {
            string_table,
            globals,
            env_stack: Vec::new(),
        }
    }
    
    pub fn string_table(&self) -> &StringTable { self.string_table }
    
    // Environments and Variable Lookup
    
    pub fn global_env(&self) -> &Environment<'r> { &self.globals }
    pub fn global_env_mut(&mut self) -> &mut Environment<'r> { &mut self.globals }
    
    pub fn local_env(&self) -> &Environment<'r> {
        self.env_stack.last().unwrap_or(&self.globals)
    }
    pub fn local_env_mut(&mut self) -> &mut Environment<'r> {
        self.env_stack.last_mut().unwrap_or(&mut self.globals)
    }
    
    pub fn push_env(&'r mut self) {
        let new_env = Environment { 
            string_table: self.string_table,
            namespace: new_namespace(),
        };
        
        self.env_stack.push(new_env);
    }
    
    pub fn pop_env(&mut self) {
        self.env_stack.pop(); // TODO close upvalues
    }
    
    pub fn lookup_value(&self, name: StringValue) -> Option<Variant> {
        for local_env in self.env_stack.iter().rev() {
            let value = local_env.lookup_value(name);
            if value.is_some() {
                return value;
            }
        }
        self.globals.lookup_value(name)
    }
    
    // returns the innermost env that contains the name, or None
    pub fn lookup_env(&self, name: StringValue) -> Option<&Environment<'r>> {
        if let Some(index) = self.search_env(name) {
            Some(&self.env_stack[index])
        } else if self.globals.has_name(name) {
            Some(&self.globals)
        } else {
            None
        }
    }
    
    pub fn lookup_env_mut(&mut self, name: StringValue) -> Option<&mut Environment<'r>> {
        if let Some(index) = self.search_env(name) {
            Some(&mut self.env_stack[index])
        } else if self.globals.has_name(name) {
            Some(&mut self.globals)
        } else {
            None
        }
    }
    
    fn search_env(&self, name: StringValue) -> Option<usize> {
        for (idx, local_env) in self.env_stack.iter().enumerate().rev() {
            if local_env.has_name(name) {
                return Some(idx);
            }
        }
        return None;
    }
}


#[derive(Debug)]
pub struct Environment<'r> {
    // TODO name for traceback
    string_table: &'r StringTable,
    namespace: Namespace<'r>,
}

impl<'r> Environment<'r> {
    pub fn has_name(&self, name: StringValue) -> bool {
        let name_key = StringKey::new(name, self.string_table, self.namespace.hasher());
        self.namespace.contains_key(&name_key)
    }
    
    pub fn lookup_value(&self, name: StringValue) -> Option<Variant> {
        let name_key = StringKey::new(name, self.string_table, self.namespace.hasher());
        self.namespace.get(&name_key).map(|value| *value)
    }
    
    pub fn store_value(&mut self, name: StringValue, value: Variant) -> Option<Variant> {
        let name_key = StringKey::new(name, self.string_table, self.namespace.hasher());
        self.namespace.insert(name_key, value)
    }
    
    // pub fn _name()
}
