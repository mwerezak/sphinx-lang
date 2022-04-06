///! Modules are the top level environment in Sphinx.
///! All top-level names live in a module, there are no "universal" global variables.
///!
///! They are also the top-level unit of execution. 
///! All Sphinx programs produce a module as a result of execution (even if it is discarded). 
///! Importing a Sphinx module simply means executing a Sphinx sub-program and binding the
///! resulting module to a name.

use std::fmt;
use std::path::PathBuf;
use std::cell::{RefCell, Ref, RefMut};
use std::hash::{Hash, Hasher, BuildHasher};
use crate::source::ModuleSource;
use crate::language::FloatType;
use crate::runtime::{Variant, HashMap, DefaultBuildHasher};
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::function::Function;
use crate::runtime::strings::StringSymbol;
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};

pub use crate::codegen::{ProgramData, Constant, Chunk, FunctionProto, ConstID, FunctionID};


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
    store: HashMap<StringSymbol, Variable>,
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
            self.create(*name, variable.access, variable.value)
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
    
    pub fn allocate() -> GC<Self> {
        GC::new(Self::default())
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
    ident: ModuleIdent,
    display: String,
    source: Option<ModuleSource>,
    data: ProgramData,
    globals: GC<GlobalEnv>,
}

impl GCTrace for Module { }

impl Module {
    pub fn allocate(source: Option<ModuleSource>, data: ProgramData) -> GC<Self> {
        let globals = GlobalEnv::allocate();
        Self::with_env(source, data, globals)
    }
    
    pub fn with_env(source: Option<ModuleSource>, data: ProgramData, globals: GC<GlobalEnv>) -> GC<Self> {
        let ident = 
            if let Some(source) = source.as_ref() { ModuleIdent::from(source) }
            else { ModuleIdent::from(globals) };
        
        let display = ident.to_string();
        
        let module = Self {
            ident,
            display,
            source,
            data,
            globals,
        };
        
        GC::new(module)
    }
    
    pub fn ident(&self) -> &ModuleIdent { &self.ident }
    
    pub fn source(&self) -> Option<&ModuleSource> { self.source.as_ref() }
    
    #[inline(always)]
    pub fn data(&self) -> &ProgramData { &self.data }
    
    #[inline(always)]
    pub fn globals(&self) -> &GlobalEnv { &self.globals }
    
    #[inline]
    pub fn get_const(&self, cid: ConstID) -> Variant {
        match self.data.get_const(cid) {
            Constant::Integer(value) => Variant::from(*value),
            
            Constant::Float(bytes) => FloatType::from_le_bytes(*bytes).into(),
            
            Constant::String(idx) => Variant::from(*self.data.get_string(*idx)),
        }
    }
    
    #[inline]
    pub fn get_function(&self, fun_id: FunctionID) -> &FunctionProto {
        self.data.get_function(fun_id)
    }
}

impl fmt::Display for Module {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(&self.display)
    }
}


/// Used to uniquely identify a module
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ModuleIdent {
    SourcePath(PathBuf), // canonicalized path to the source file
    SourceHash(u64),  // hash of source text
    RefHash(u64),  // GC address of globals, for Native Modules
}

lazy_static! {
    static ref IDENT_HASH: DefaultBuildHasher = DefaultBuildHasher::default();
}

impl From<&ModuleSource> for ModuleIdent {
    fn from(source: &ModuleSource) -> Self {
        match source {
            ModuleSource::File(path) => {
                Self::SourcePath(path.canonicalize().expect("invalid source path"))
            },
            
            ModuleSource::String(text) => {
                let mut state = IDENT_HASH.build_hasher();
                text.hash(&mut state);
                let hash = state.finish();
                Self::SourceHash(hash)
            }
        }
    }
}

impl From<GC<GlobalEnv>> for ModuleIdent {
    fn from(env: GC<GlobalEnv>) -> Self {
        let mut state = IDENT_HASH.build_hasher();
        <GC<GlobalEnv> as Hash>::hash(&env, &mut state);
        let hash = state.finish();
        Self::RefHash(hash)
    }
}

impl fmt::Display for ModuleIdent {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SourcePath(path) => {
                let prefix = std::env::current_dir().ok()
                    .map(|pwd| pwd.canonicalize().ok()).flatten();
                
                let path = prefix.map(|prefix| path.strip_prefix(prefix).ok()).flatten()
                    .unwrap_or(path);
                
                write!(fmt, "\"{}\"", path.display())
            },
            
            Self::SourceHash(hash) => write!(fmt, "#{:016X}", hash),
            
            Self::RefHash(hash) => write!(fmt, "&{:016X}", hash),
        }
    }
}