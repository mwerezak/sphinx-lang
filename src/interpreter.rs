use std::cell::RefCell;
use std::collections::HashMap;

use crate::parser::stmt::Label;
use crate::runtime::{Variant, VariantKey, DefaultBuildHasher};
use crate::runtime::strings::{StringKey, StringValue};
use crate::runtime::string_table::StringTable;

pub mod eval;
pub mod exec;

pub use eval::EvalContext;
pub use exec::ExecContext;


#[derive(Debug, Clone)]
pub enum ControlFlow {
    None,
    Continue(Option<Label>),
    Break(Option<Label>, Variant),
    Return(Variant),
}

pub type Dictionary<'s> = HashMap<VariantKey<'s>, Variant, DefaultBuildHasher>;
pub fn new_dictionary<'s>() -> Dictionary<'s> {
    Dictionary::with_hasher(DefaultBuildHasher::default())
}


pub enum Access {
    ReadOnly,
    ReadWrite,
}

pub struct Variable {
    pub access: Access,
    pub value: Variant,
}

pub type Namespace<'s> = HashMap<StringKey<'s>, Variant, DefaultBuildHasher>;

pub fn new_namespace<'s>() -> Namespace<'s> {
    Namespace::with_hasher(DefaultBuildHasher::default())
}



pub fn new_root_env<'r, 's>(string_table: &'s StringTable) -> Environment<'r, 's> {
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
    string_table: &'s StringTable,
}

impl<'r, 's> Environment<'r, 's> {
    /// Create a new local Environment with this one as it's parent.
    pub fn new_local<'a>(&'a self) -> Environment<'a, 's> {
        Environment {
            parent: Some(self),
            namespace: RefCell::new(new_namespace()),
            string_table: self.string_table,
        }
    }
    
    pub fn string_table(&self) -> &StringTable { self.string_table }
    
    /// Check if the name exists in this Environment
    pub fn has_name(&self, name: &StringValue) -> bool {
        let local_store = self.namespace.borrow();
        let name_key = StringKey::new(name.clone(), self.string_table);
        local_store.contains_key(&name_key)
    }
    
    /// Find the innermost Environment that has the given name, or None
    pub fn find_name<'a>(&'a self, name: &StringValue) -> Option<&'a Environment<'r, 's>> {
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
    pub fn lookup_value(&self, name: &StringValue) -> Option<Variant> {
        let local_store = self.namespace.borrow();
        let name_key = StringKey::new(name.clone(), self.string_table);
        local_store.get(&name_key).map(|value| value.clone())
    }
    
    /// Lookup a value for the given name in the innermost Environment in which it can be found.
    pub fn find_value(&self, name: &StringValue) -> Option<Variant> {
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
    pub fn insert_value(&self, name: &StringValue, value: &Variant) -> Option<Variant> {
        let mut local_store = self.namespace.borrow_mut();
        let name_key = StringKey::new(name.clone(), self.string_table);
        local_store.insert(name_key, value.clone())
    }
    
    pub fn remove_value(&self, name: &StringValue) -> Option<Variant> {
        let mut local_store = self.namespace.borrow_mut();
        let name_key = StringKey::new(name.clone(), self.string_table);
        local_store.remove(&name_key)
    }
}
