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
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::types::function::Function;
use crate::runtime::strings::StringSymbol;
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
    pub fn allocate() -> GC<Self> {
        GC::allocate(Self::default())
    }
    
    pub fn borrow(&self) -> Ref<Namespace> {
        self.namespace.borrow()
    }
    
    pub fn borrow_mut(&self) -> RefMut<Namespace> {
        self.namespace.borrow_mut()
    }
}

impl GCTrace for GlobalEnv { }



#[derive(Debug)]
pub struct Module {
    name: String,
    source: Option<ModuleSource>,
    data: ProgramData,
    globals: GC<GlobalEnv>,
}

impl GCTrace for Module {
    fn extra_size(&self) -> usize {
        std::mem::size_of_val(self.name.as_str())
    }
}

impl Module {
    pub fn allocate(source: Option<ModuleSource>, data: ProgramData) -> GC<Self> {
        let globals = GlobalEnv::allocate();
        Self::with_globals(globals, source, data)
    }
    
    pub fn with_globals(globals: GC<GlobalEnv>, source: Option<ModuleSource>, data: ProgramData) -> GC<Self> {
        let module = Self {
            name: Self::get_name(source.as_ref()),
            source,
            data,
            globals,
        };
        GC::allocate(module)
    }
    
    pub fn name(&self) -> &str { self.name.as_str() }
    
    pub fn source(&self) -> Option<&ModuleSource> { self.source.as_ref() }
    
    #[inline(always)]
    pub fn data(&self) -> &ProgramData { &self.data }
    
    #[inline(always)]
    pub fn globals(&self) -> &GlobalEnv { &self.globals }
    
    #[inline]
    pub fn get_const(self_module: GC<Self>, cid: ConstID) -> Variant {
        match *self_module.data.get_const(cid) {
            Constant::Integer(value) => Variant::from(value),
            
            Constant::Float(bytes) => FloatType::from_le_bytes(bytes).into(),
            
            Constant::String(idx) => Variant::from(*self_module.data.get_string(idx)),
            
            Constant::Function(chunk_id, function_id) => {
                let signature = self_module.data.get_signature(function_id);
                let function = Function::new(signature.clone(), self_module, chunk_id);
                Variant::from(function)
            }
        }
    }
    
    fn get_name(source: Option<&ModuleSource>) -> String {
        if let Some(ModuleSource::File(path)) = source {
            path.display().to_string()
        }
        else {
            "<anonymous module>".to_string()
        }
    }
}
