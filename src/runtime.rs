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

use strings::{StringTableCell, StringKey, StringValue};


// Default Hasher

pub type DefaultHasher = AHasher;
pub type DefaultBuildHasher = ahash::RandomState;
pub type Dictionary<'s> = HashMap<VariantKey<'s>, Variant, DefaultBuildHasher>;
pub type Namespace<'s> = HashMap<StringKey<'s>, Variant, DefaultBuildHasher>;

pub fn new_dictionary<'s>() -> Dictionary<'s> {
    Dictionary::with_hasher(DefaultBuildHasher::default())
}

pub fn new_namespace<'s>() -> Namespace<'s> {
    Namespace::with_hasher(DefaultBuildHasher::default())
}


// #[derive(Debug)]
// pub struct Runtime<'r, 's> {
//     string_table: &'s StringTable,
    
//     // TODO when modules are implemented this will be replaced
//     root_env: Environment<'r, 's>,
// }

// impl<'s> Runtime<'_, 's> {
//     pub fn new(string_table: &'s StringTable) -> Self {
//         let root_env = Environment { 
//             string_table, 
//             namespace: new_namespace(),
//             parent: None,
//         };
        
//         Runtime {
//             string_table,
//             root_env,
//         }
//     }
    
//     pub fn string_table(&self) -> &StringTable { self.string_table }
    
// }

pub fn placeholder_new_env<'r, 's>(string_table: &'s StringTableCell) -> Environment<'r, 's> {
    Environment {
        parent: None,
        namespace: RefCell::new(new_namespace()),
        string_table,
    }
}

#[derive(Debug)]
pub struct Environment<'r, 's> {
    parent: Option<&'r Environment<'r, 's>>,
    namespace: RefCell<Namespace<'s>>,
    string_table: &'s StringTableCell,
}

impl<'r, 's> Environment<'r, 's> {
    pub fn string_table(&self) -> &StringTableCell { self.string_table }
    
    /// Check if the name exists in this Environment
    pub fn has_name(&self, name: StringValue) -> bool {
        let local_store = self.namespace.borrow();
        let name_key = StringKey::new(name, self.string_table);
        local_store.contains_key(&name_key)
    }
    
    /// Find the innermost Environment that has the given name, or None
    pub fn find_name<'a>(&'a self, name: StringValue) -> Option<&'a Environment<'r, 's>> {
        let mut next_env = Some(self);
        while let Some(env) = next_env {
            if env.has_name(name) {
                return next_env
            }
            next_env = self.parent;
        }
        None
    }
    
    /// Lookup a value for the given name in this Environment
    pub fn lookup_value(&self, name: StringValue) -> Option<Variant> {
        let local_store = self.namespace.borrow();
        let name_key = StringKey::new(name, self.string_table);
        local_store.get(&name_key).map(|value| *value)
    }
    
    /// Lookup a value for the given name in the innermost Environment in which it can be found.
    pub fn find_value(&self, name: StringValue) -> Option<Variant> {
        let mut next_env = Some(self);
        while let Some(env) = next_env {
            let value = env.lookup_value(name);
            if value.is_some() {
                return value;
            }
            next_env = self.parent;
        }
        None
    }
    
    /// Store a value in this Environment
    pub fn insert_value(&self, name: StringValue, value: Variant) -> Option<Variant> {
        let mut local_store = self.namespace.borrow_mut();
        let name_key = StringKey::new(name, self.string_table);
        local_store.insert(name_key, value)
    }
    
    pub fn remove_value(&self, name: StringValue) -> Option<Variant> {
        let mut local_store = self.namespace.borrow_mut();
        let name_key = StringKey::new(name, self.string_table);
        local_store.remove(&name_key)
    }
}
