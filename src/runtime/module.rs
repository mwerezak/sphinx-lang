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
use crate::runtime::{Variant, DefaultBuildHasher};
use crate::runtime::gc::GCObject;
use crate::runtime::strings::StringSymbol;
use crate::runtime::types::function::Function;
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};

pub use crate::codegen::{ProgramData, Constant, ConstID, ChunkID};


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Access {
    ReadOnly,
    ReadWrite,
}

#[derive(Debug, Clone)]
pub struct Variable {
    access: Access,
    value: Variant,
}

#[derive(Debug, Clone)]
pub struct Namespace {
    store: HashMap<StringSymbol, Variable, DefaultBuildHasher>,
}

impl Default for Namespace {
    fn default() -> Self { Self::new() }
}

impl Namespace {
    pub fn new() -> Self {
        Self { 
            store: HashMap::with_hasher(DefaultBuildHasher::default()),
        }
    }
    
    // if the variable already exists, it is overwritten
    pub fn create(&mut self, name: StringSymbol, access: Access, value: Variant) {
        self.store.insert(name, Variable { access, value });
    }
    
    pub fn delete(&mut self, name: &StringSymbol) -> ExecResult<()> {
        if self.store.remove(name).is_none() {
            return Err(ErrorKind::NameNotDefined(name.to_string()).into())
        }
        Ok(())
    }
    
    pub fn lookup<'a>(&'a self, name: &StringSymbol) -> ExecResult<&'a Variant> {
        self.store.get(name)
            .map(|var| &var.value)
            .ok_or_else(|| ErrorKind::NameNotDefined(name.to_string()).into())
    }
    
    pub fn lookup_mut<'a>(&'a mut self, name: &StringSymbol) -> ExecResult<&'a mut Variant> {
        let variable = self.store.get_mut(name)
            .ok_or_else(|| RuntimeError::from(ErrorKind::NameNotDefined(name.to_string())))?;
            
        if variable.access != Access::ReadWrite {
            return Err(ErrorKind::CantAssignImmutable.into());
        }
        
        Ok(&mut variable.value)
    }
    
    pub fn extend(&mut self, other: &Namespace) {
        for (name, variable) in other.store.iter() {
            self.create(*name, variable.access, variable.value.clone())
        }
    }
}


#[derive(Debug, Default, Clone)]
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
        Self::default()
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
    name: Option<String>,
    source: Option<ModuleSource>,
    data: ProgramData,
    globals: GlobalEnv,
}

impl Module {
    pub fn module_id(&self) -> ModuleID { self.id }
    
    pub fn source(&self) -> Option<&ModuleSource> { self.source.as_ref() }
    
    pub fn data(&self) -> &ProgramData { &self.data }
    
    pub fn globals(&self) -> &GlobalEnv { &self.globals }
    
    #[inline]
    pub fn get_const(&self, cid: ConstID) -> Variant {
        match *self.data.get_const(cid) {
            Constant::Integer(value) => Variant::from(value),
            Constant::Float(bytes) => FloatType::from_le_bytes(bytes).into(),
            Constant::String(idx) => Variant::from(*self.data.get_string(idx)),
            Constant::Function(chunk_id, function_id) => {
                let signature = self.data.get_signature(function_id);
                let function = Function::new(signature.clone(), self.id, chunk_id);
                let obj = GCObject::from(function);
                Variant::from(obj.allocate())
            }
        }
    }
    
    
}

#[derive(Debug)]
pub struct ModuleCache {
    modules: HashMap<ModuleID, Module, DefaultBuildHasher>,
    id_hasher: DefaultBuildHasher,
}

impl Default for ModuleCache {
    fn default() -> Self { Self::new() }
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
    
    pub fn insert(&mut self, data: ProgramData, name: Option<String>, source: Option<ModuleSource>) -> ModuleID {
        self.insert_with_globals(data, name, source, GlobalEnv::new())
    }
    
    pub fn insert_with_globals(&mut self, data: ProgramData, name: Option<String>, source: Option<ModuleSource>, globals: GlobalEnv) -> ModuleID {
        let module_id = self.new_module_id(name.as_deref(), source.as_ref());
        
        let module = Module {
            id: module_id,
            name, source, data,
            globals,
        };
        
        self.modules.insert(module_id, module);
        
        module_id
    }
    
    /// Get a new, unused module ID.
    fn new_module_id(&self, name: Option<&str>, source: Option<&ModuleSource>) -> ModuleID {
        
        let mut new_id;
        if let Some(name) = name {
            let mut state = self.id_hasher.build_hasher();
            name.hash(&mut state);
            new_id = state.finish();
        } else if let Some(source) = source {
            let mut state = self.id_hasher.build_hasher();
            source.hash(&mut state);
            new_id = state.finish();
        } else {
            new_id = 0;
        }
        
        while self.modules.contains_key(&new_id) {
            new_id = new_id.wrapping_add(1);
        }
        new_id
    }
}
