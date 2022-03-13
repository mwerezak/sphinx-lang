use std::sync::{Arc, RwLock, RwLockWriteGuard};
use std::ops::{Deref, DerefMut};
use std::collections::HashMap;

use crate::parser::stmt::Label;
use crate::runtime::{Variant, VariantKey, DefaultBuildHasher};
use crate::runtime::strings::{StringSymbol};
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

pub type Namespace = HashMap<StringSymbol, Variable, DefaultBuildHasher>;


#[derive(Debug)]
pub struct Environment {
    parent: Option<Arc<Environment>>,
    namespace: RwLock<Namespace>,  // if we switch to Arc, use a RwLock for this
}

impl Environment {
    pub fn new_root() -> Arc<Self> {
        let namespace = Namespace::with_hasher(DefaultBuildHasher::default());
        let root_env = Self {
            parent: None,
            namespace: RwLock::new(namespace),
        };
        Arc::new(root_env)
    }

    /// Create a new local Environment with this one as it's parent.
    pub fn new_local(self: &Arc<Self>) -> Arc<Self> {
        let namespace = Namespace::with_hasher(DefaultBuildHasher::default());
        let local_env = Self {
            parent: Some(self.clone()),
            namespace: RwLock::new(namespace),
        };
        Arc::new(local_env)
    }
    
    
    // if the variable already exists, it is overwritten
    pub fn create(&self, name: StringSymbol, access: Access, value: Variant) -> ExecResult<()> {
        let mut local_store = self.namespace.write().unwrap();
        let variable = Variable { access, value: value.clone() };
        
        local_store.insert(name, variable);
        Ok(())
    }
    

    pub fn delete(&self, name: &StringSymbol) -> ExecResult<()> {
        let mut local_store = self.namespace.write().unwrap();
        if !local_store.contains_key(&name) {
            return Err(ErrorKind::NameNotDefinedLocal(name.to_string()).into())
        }
        
        local_store.remove(&name);
        Ok(())
    } 
    
    pub fn lookup_local(&self, name: &StringSymbol) -> ExecResult<Variant> {
        self.get_value(&name)
            .ok_or_else(|| ErrorKind::NameNotDefinedLocal(name.to_string()).into())
    }
    
    /// Lookup a value for the given name in this Environment
    fn get_value(&self, name: &StringSymbol) -> Option<Variant> {
        let local_store = self.namespace.read().unwrap();
        local_store.get(&name).map(|var| var.value.clone())
    }
    
    pub fn lookup(self: &Arc<Self>, name: &StringSymbol) -> ExecResult<Variant> {
        self.find_value(&name)
            .ok_or_else(|| ErrorKind::NameNotDefined(name.to_string()).into())
    }
    
    /// Lookup a value for the given name in the innermost Environment in which it can be found.
    fn find_value(self: &Arc<Self>, name: &StringSymbol) -> Option<Variant> {
        let mut next_env = Some(self);
        while let Some(env) = next_env {
            let value = env.get_value(name);
            if value.is_some() {
                return value;
            }
            next_env = env.parent.as_ref();
        }
        None
    }
    
    pub fn lookup_local_mut(&self, name: &StringSymbol) -> ExecResult<VariantWrite<'_>> {
        self.get_write(name)
    }
    
    fn get_write(&self, name: &StringSymbol) -> ExecResult<VariantWrite>{
        let local_store = self.namespace.write().unwrap();
        match local_store.get(&name) {
            Some(variable) if variable.access != Access::ReadWrite
                => Err(ErrorKind::CantAssignImmutable.into()),
            
            Some(..) => Ok(VariantWrite { name: *name, write: local_store }),
                
            None => Err(ErrorKind::NameNotDefinedLocal(name.to_string()).into()),
        }
    }
    
    pub fn lookup_mut<'a>(self: &'a Arc<Self>, name: &StringSymbol) -> ExecResult<VariantWrite<'a>> {
        self.find_write(name)
    }
    
    /// Return a write guard on the closest namespace that contains the name name
    fn find_write<'a>(self: &'a Arc<Self>, name: &StringSymbol) -> ExecResult<VariantWrite<'a>> {
        let mut next_env = Some(self);
        while let Some(env) = next_env {
            match env.get_write(name) {
                Err(error) if matches!(error.kind(), ErrorKind::NameNotDefinedLocal(..)) => {
                    next_env = env.parent.as_ref();
                },
                // Err(error) => return Err(error),
                // Ok(write) => return Ok(write),
                result => return result,
            }
        }
        Err(ErrorKind::NameNotDefined(name.to_string()).into())
    }
}

// Helper struct
pub struct VariantWrite<'a> {
    // name: StringSymbol,
    name: StringSymbol,
    write: RwLockWriteGuard<'a, Namespace>,
}

impl Deref for VariantWrite<'_> {
    type Target = Variant;
    fn deref(&self) -> &Self::Target { 
        &self.write.get(&self.name).unwrap().value
    }
}

impl DerefMut for VariantWrite<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target { 
        &mut self.write.get_mut(&self.name).unwrap().value
    }
}