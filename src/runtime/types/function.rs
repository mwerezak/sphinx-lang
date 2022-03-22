use crate::parser::lvalue::DeclType;
use crate::codegen::{ChunkID, ConstID};
use crate::runtime::module::{ModuleID, Access};
use crate::runtime::gc::GCObject;


pub struct Function {
    signature: Signature,
    module_id: ModuleID,
    chunk_id: ChunkID,
}

impl Function {
    pub fn new(signature: Signature, module_id: ModuleID, chunk_id :ChunkID) -> Self {
        Self { signature, module_id, chunk_id }
    }
}

impl From<Function> for GCObject {
    fn from(function: Function) -> Self {
        Self::Function(Box::new(function))
    }
}


#[derive(Clone, Debug)]
pub struct Signature {
    name: Option<ConstID>,
    required: Box<[Parameter]>,
    default: Box<[Parameter]>,
    variadic: Option<Parameter>,
}

impl Signature {
    pub fn new(name: Option<ConstID>, required: Vec<Parameter>, default: Vec<Parameter>, variadic: Option<Parameter>) -> Self {
        Self {
            name,
            required: required.into_boxed_slice(),
            default: default.into_boxed_slice(),
            variadic,
        }
    }
    
    pub fn name(&self) -> Option<ConstID> {
        self.name
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
}

// TODO don't create an entire chunk for each default argument

impl Parameter {
    pub fn new(name: ConstID, decl: DeclType) -> Self {
        Self { name, decl }
    }
    
    pub fn name(&self) -> &ConstID { &self.name }
    pub fn decl(&self) -> &DeclType { &self.decl }
}