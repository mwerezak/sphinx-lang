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
use crate::codegen::{Program, UnloadedProgram};
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


pub type ModuleID = u64;

#[derive(Debug)]
pub struct Module {
    source: ModuleSource,
    program: Program,
    globals: RefCell<Namespace>,
}

impl Module {
    pub fn module_id(&self) -> ModuleID { self.program.module_id() }
    pub fn source(&self) -> &ModuleSource { &self.source }
    
    #[inline(always)]
    pub fn program(&self) -> &Program { &self.program }
    
    pub fn globals(&self) -> Ref<Namespace> { self.globals.borrow() }
    pub fn globals_mut(&self) -> RefMut<Namespace> { self.globals.borrow_mut() }
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
    
    /// Loads an `UnloadedProgram`, creating a new module for it
    pub fn load(&mut self, program: UnloadedProgram, source: ModuleSource) -> &Module {
        let module_id = self.new_module_id(&source);
        let program = Program::load(program, module_id);
        
        let module = Module {
            source, program,
            globals: RefCell::new(Namespace::new()),
        };
        
        self.modules.insert(module_id, module);
        self.modules.get(&module_id).unwrap()
    }
    
    /// Get a new, unused module ID.
    fn new_module_id(&mut self, source: &ModuleSource) -> ModuleID {
        let mut state = self.id_hasher.build_hasher();
        source.hash(&mut state);
        let mut new_id = state.finish();
        
        while self.modules.contains_key(&new_id) {
            new_id = new_id.wrapping_add(1);
        }
        new_id
    }
    
    pub fn get(&self, module_id: &ModuleID) -> Option<&Module> {
        self.modules.get(module_id)
    }
}