use crate::codegen::ChunkID;
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
    name: StringSymbol,
    access: Access,
    default: Option<ChunkID>,
}

impl Parameter {
    pub fn name(&self) -> &StringSymbol { &self.name }
    pub fn access(&self) -> &Access { &self.access }
    pub fn has_default(&self) -> bool { self.default.is_some() }
}