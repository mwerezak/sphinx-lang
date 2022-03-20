use crate::parser::lvalue::DeclType;
use crate::codegen::{ChunkID, ConstID};
use crate::runtime::module::{ModuleID, Access};
use crate::runtime::strings::StringSymbol;


pub struct Function {
    signature: Signature,
    module_id: ModuleID,
    chunk_id: ChunkID,
}

#[derive(Clone, Debug)]
pub struct Signature {
    // all parameters are positional
    required: Box<[Parameter]>,
    default: Box<[Parameter]>,
    variadic: Option<Parameter>,
}

impl Signature {
    pub fn new(required: Vec<Parameter>, default: Vec<Parameter>, variadic: Option<Parameter>) -> Self {
        Self {
            required: required.into_boxed_slice(),
            default: default.into_boxed_slice(),
            variadic,
        }
    }
    
    pub fn is_variadic(&self) -> bool { 
        self.variadic.is_some()
    }
    
    pub fn min_arity(&self) -> usize { 
        self.required.len()
    }
    
    pub fn max_arity(&self) -> Option<usize> {
        if self.is_variadic() { None }
        else { Some(self.required.len() + self.default.len()) }
    }
}

#[derive(Clone, Debug)]
pub struct Parameter {
    name: ConstID,
    decl: DeclType,
    default: Option<ChunkID>,
}

impl Parameter {
    pub fn new(name: ConstID, decl: DeclType, default: Option<ChunkID>) -> Self {
        Self { name, decl, default }
    }
    
    pub fn name(&self) -> &ConstID { &self.name }
    pub fn decl(&self) -> &DeclType { &self.decl }
    pub fn has_default(&self) -> bool { self.default.is_some() }
}