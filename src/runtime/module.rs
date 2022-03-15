///! Modules are the top level environment in Sphinx.
///! All top-level names live in a module, there are no "universal" global variables.
///!
///! They are also the top-level unit of execution. 
///! All Sphinx programs produce a module as a result of execution (even if it is discarded). 
///! Importing a Sphinx module simply means executing a Sphinx sub-program and binding the
///! resulting module to a name.

use std::collections::HashMap;
use crate::runtime::{Variant, DefaultBuildHasher};
use crate::runtime::strings::StringSymbol;
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Access {
    ReadOnly,
    ReadWrite,
}

#[derive(Debug)]
pub struct Variable {
    access: Access,
    value: Variant,
}

#[derive(Debug)]
pub struct Namespace {
    store: HashMap<StringSymbol, Variable, DefaultBuildHasher>,
}

impl Namespace {
    pub fn new() -> Self {
        Self { 
            store: HashMap::with_hasher(DefaultBuildHasher::default()),
        }
    }
    
    // if the variable already exists, it is overwritten
    pub fn create(&mut self, name: StringSymbol, access: Access, value: Variant) -> ExecResult<()> {
        self.store.insert(name, Variable { access, value });
        Ok(())
    }
    
    pub fn delete(&mut self, name: &StringSymbol) -> ExecResult<()> {
        if self.store.remove(&name).is_none() {
            return Err(ErrorKind::NameNotDefined(name.to_string()).into())
        }
        Ok(())
    }
    
    pub fn lookup<'a>(&'a self, name: &StringSymbol) -> ExecResult<&'a Variant> {
        self.store.get(&name)
            .map(|var| &var.value)
            .ok_or_else(|| ErrorKind::NameNotDefined(name.to_string()).into())
    }
    
    pub fn lookup_mut<'a>(&'a mut self, name: &StringSymbol) -> ExecResult<&'a mut Variant> {
        let variable = self.store.get_mut(&name)
            .ok_or_else(|| RuntimeError::from(ErrorKind::NameNotDefined(name.to_string())))?;
            
        if variable.access != Access::ReadWrite {
            return Err(ErrorKind::CantAssignImmutable.into());
        }
        
        Ok(&mut variable.value)
    }
}


pub struct Module {
    globals: Namespace,
}