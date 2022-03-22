///! Modules are the top level environment in Sphinx.
///! All top-level names live in a module, there are no "universal" global variables.
///!
///! They are also the top-level unit of execution. 
///! All Sphinx programs produce a module as a result of execution (even if it is discarded). 
///! Importing a Sphinx module simply means executing a Sphinx sub-program and binding the
///! resulting module to a name.

use std::cell::{RefCell, Ref, RefMut};
use std::hash::{Hash, Hasher, BuildHasher};
use std::collections::HashMap;
use crate::source::ModuleSource;
use crate::language::FloatType;
use crate::codegen::{ProgramData, Constant, ConstID};
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


#[derive(Debug)]
pub struct GlobalEnv {
    namespace: RefCell<Namespace>,
}

impl From<Namespace> for GlobalEnv {
    fn from(namespace: Namespace) -> Self {
        Self { namespace: RefCell::new(namespace) }
    }
}

impl GlobalEnv {
    pub fn new() -> Self {
        Self::from(Namespace::new())
    }
    
    pub fn borrow(&self) -> Ref<Namespace> {
        self.namespace.borrow()
    }
    
    pub fn borrow_mut(&self) -> RefMut<Namespace> {
        self.namespace.borrow_mut()
    }
}


pub type ModuleID = u64;

#[derive(Debug)]
pub struct Module {
    id: ModuleID,
    source: ModuleSource,
    data: ProgramData,
    globals: GlobalEnv,
}

impl Module {
    pub fn module_id(&self) -> ModuleID { self.id }
    
    pub fn source(&self) -> &ModuleSource { &self.source }
    
    pub fn data(&self) -> &ProgramData { &self.data }
    
    pub fn globals(&self) -> &GlobalEnv { &self.globals }
    
    #[inline]
    pub fn get_const(&self, cid: ConstID) -> Variant {
        match *self.data.get_const(cid) {
            Constant::Integer(value) => Variant::from(value),
            Constant::Float(bytes) => FloatType::from_le_bytes(bytes).into(),
            Constant::String(idx) => Variant::from(*self.data.get_string(idx)),
            Constant::Function(chunk_id, function_id) => {
                unimplemented!()
            }
        }
    }
}

#[derive(Debug)]
pub struct ModuleCache {
    modules: HashMap<ModuleID, Module, DefaultBuildHasher>,
    id_hasher: DefaultBuildHasher,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            modules: HashMap::with_hasher(DefaultBuildHasher::default()),
            id_hasher: DefaultBuildHasher::default(),
        }
    }
    
    pub fn get(&self, module_id: &ModuleID) -> Option<&Module> {
        self.modules.get(module_id)
    }
    
    /// Create a new module
    pub fn insert(&mut self, source: ModuleSource, data: ProgramData) -> ModuleID {
        let module_id = self.new_module_id(&source);
        let module = Module {
            id: module_id,
            globals: GlobalEnv::new(),
            source, data,
        };
        
        self.modules.insert(module_id, module);
        
        module_id
    }
    
    /// Get a new, unused module ID.
    fn new_module_id(&self, source: &ModuleSource) -> ModuleID {
        let mut state = self.id_hasher.build_hasher();
        source.hash(&mut state);
        let mut new_id = state.finish();
        
        while self.modules.contains_key(&new_id) {
            new_id = new_id.wrapping_add(1);
        }
        new_id
    }
}
