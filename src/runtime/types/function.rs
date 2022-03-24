use crate::parser::lvalue::DeclType;
use crate::codegen::{ChunkID, ConstID};
use crate::runtime::module::{ModuleID, Access};
use crate::runtime::strings::StringSymbol;
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
    name: Option<StringSymbol>,
    required: Box<[Parameter]>,
    default: Box<[Parameter]>,
    variadic: Option<Parameter>,
}

impl Signature {
    pub fn new(name: Option<StringSymbol>, required: Vec<Parameter>, default: Vec<Parameter>, variadic: Option<Parameter>) -> Self {
        Self {
            name,
            required: required.into_boxed_slice(),
            default: default.into_boxed_slice(),
            variadic,
        }
    }
    
    pub fn name(&self) -> Option<StringSymbol> { self.name }
    pub fn required(&self) -> &[Parameter] { &self.required }
    pub fn default(&self) -> &[Parameter] { &self.default }
    pub fn variadic(&self) -> Option<&Parameter> { self.variadic.as_ref() }
    
    pub fn min_arity(&self) -> usize {
        self.required.len()
    }
    
    pub fn max_arity(&self) -> Option<usize> {
        if self.variadic().is_some() { None }
        else { Some(self.required.len() + self.default.len()) }
    }
}

#[derive(Clone, Debug)]
pub struct Parameter {
    name: StringSymbol,
    decl: DeclType,
}

// TODO don't create an entire chunk for each default argument

impl Parameter {
    pub fn new(name: StringSymbol, decl: DeclType) -> Self {
        Self { name, decl }
    }
    
    pub fn name(&self) -> &StringSymbol { &self.name }
    pub fn decl(&self) -> &DeclType { &self.decl }
}