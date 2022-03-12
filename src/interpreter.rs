use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};
use std::ops::DerefMut;
use std::collections::HashMap;

use crate::parser::stmt::Label;
use crate::runtime::{Variant, VariantKey, DefaultBuildHasher};
use crate::runtime::strings::{StringKey, StringValue};
use crate::runtime::string_table::StringTable;
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};

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


// Environments

// pub type Dictionary<'s> = HashMap<VariantKey<'s>, Variant, DefaultBuildHasher>;
// pub fn new_dictionary<'s>() -> Dictionary<'s> {
//     Dictionary::with_hasher(DefaultBuildHasher::default())
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Access {
    ReadOnly,
    ReadWrite,
}

#[derive(Debug)]
pub struct Variable {
    pub access: Access,
    pub value: Variant,
}


pub type Namespace<'s> = HashMap<StringKey<'s>, Variable, DefaultBuildHasher>;


pub fn new_root_env<'r, 's>(string_table: &'s StringTable) -> Environment<'r, 's> {
    let namespace = Namespace::with_hasher(DefaultBuildHasher::default());
    Environment {
        parent: None,
        namespace: RefCell::new(namespace),
        string_table,
    }
}

#[derive(Debug)]
pub struct Environment<'r, 's> {
    parent: Option<&'r Environment<'r, 's>>,
    namespace: RefCell<Namespace<'s>>,  // if we switch to Arc, use a RwLock for this
    string_table: &'s StringTable,
}

impl<'r, 's> Environment<'r, 's> {
    /// Create a new local Environment with this one as it's parent.
    pub fn new_local<'a>(&'a self) -> Environment<'a, 's> {
        let namespace = Namespace::with_hasher(DefaultBuildHasher::default());
        Environment {
            parent: Some(self),
            namespace: RefCell::new(namespace),
            string_table: self.string_table,
        }
    }
    
    pub fn string_table(&self) -> &StringTable { self.string_table }
    
    #[inline]
    fn namespace(&self) -> Ref<Namespace<'s>> { 
        self.namespace.borrow() 
    }
    
    #[inline]
    fn namespace_mut(&self) -> RefMut<Namespace<'s>> {
        self.namespace.borrow_mut() 
    }
    
    // if the variable already exists, it is overwritten
    pub fn create(&self, name: &StringValue, access: Access, value: Variant) -> ExecResult<()> {
        let mut local_store = self.namespace_mut();
        let name_key = StringKey::new(name.clone(), self.string_table);
        let variable = Variable { access, value: value.clone() };
        
        local_store.insert(name_key, variable);
        Ok(())
    }

    pub fn delete(&self, name: &StringValue) -> ExecResult<()> {
        let mut local_store = self.namespace_mut();
        let name_key = StringKey::new(name.clone(), self.string_table);
        if !local_store.contains_key(&name_key) {
            return Err(ErrorKind::NameNotDefinedLocal(name_key.to_string()).into())
        }
        
        local_store.remove(&name_key);
        Ok(())
    } 
    
    pub fn lookup(&self, name: &StringValue) -> ExecResult<Variant> {
        let name_key = StringKey::new(name.clone(), self.string_table);
        self.find_value(&name_key)
            .ok_or_else(|| ErrorKind::NameNotDefined(name_key.to_string()).into())
    }
    
    pub fn lookup_local(&self, name: &StringValue) -> ExecResult<Variant> {
        let name_key = StringKey::new(name.clone(), self.string_table);
        self.get_value(&name_key)
            .ok_or_else(|| ErrorKind::NameNotDefinedLocal(name_key.to_string()).into())
    }
    
    pub fn lookup_mut(&self, name: &StringValue) -> ExecResult<RefMut<Variant>> {
        let name_key = StringKey::new(name.clone(), self.string_table);
        
        let variable = self.find_mut(&name_key)
            .ok_or_else(|| RuntimeError::from(ErrorKind::NameNotDefined(name_key.to_string())))?;
        
        if variable.access != Access::ReadWrite {
            return Err(ErrorKind::CantAssignImmutable.into())
        }
        
        Ok(RefMut::map(variable, |var| &mut var.value))
    }
    
    pub fn lookup_local_mut(&self, name: &StringValue) -> ExecResult<RefMut<Variant>> {
        let name_key = StringKey::new(name.clone(), self.string_table);
        
        let variable = self.get_mut(&name_key)
                .ok_or_else(|| RuntimeError::from(ErrorKind::NameNotDefinedLocal(name_key.to_string())))?;
        
        if variable.access != Access::ReadWrite {
            return Err(ErrorKind::CantAssignImmutable.into())
        }
        
        Ok(RefMut::map(variable, |var| &mut var.value))
    }
    
    /// Lookup a value for the given name in this Environment
    fn get_value(&self, name_key: &StringKey<'s>) -> Option<Variant> {
        let local_store = self.namespace();
        local_store.get(&name_key).map(|var| var.value.clone())
    }
    
    fn get_mut(&self, name_key: &StringKey<'s>) -> Option<RefMut<Variable>>{
        let local_store = self.namespace_mut();
        
        if !local_store.contains_key(&name_key) {
            return None;
        }
        
        let ref_mut = RefMut::map(local_store,
            |local_store| local_store.get_mut(&name_key).unwrap()
        );
        Some(ref_mut)
    }
    
    /// Lookup a value for the given name in the innermost Environment in which it can be found.
    fn find_value(&self, name_key: &StringKey<'s>) -> Option<Variant> {
        let mut next_env = Some(self);
        while let Some(env) = next_env {
            let value = env.get_value(name_key);
            if value.is_some() {
                return value;
            }
            next_env = env.parent;
        }
        None
    }
    
    fn find_mut(&self, name_key: &StringKey<'s>) -> Option<RefMut<Variable>> {
        let mut next_env = Some(self);
        while let Some(env) = next_env {
            let value = env.get_mut(name_key);
            if value.is_some() {
                return value;
            }
            next_env = env.parent;
        }
        None
    }
    


}
