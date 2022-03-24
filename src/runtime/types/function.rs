use std::fmt;
use crate::parser::lvalue::DeclType;
use crate::codegen::{ChunkID, ConstID};
use crate::runtime::module::{ModuleID, Access};
use crate::runtime::strings::{StringSymbol, STRING_TABLE};
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
    display: String,
    name: Option<StringSymbol>,
    required: Box<[Parameter]>,
    default: Box<[Parameter]>,
    variadic: Option<Parameter>,
}

impl Signature {
    pub fn new(name: Option<StringSymbol>, required: Vec<Parameter>, default: Vec<Parameter>, variadic: Option<Parameter>) -> Self {
        // build this once and cache the result, because it's expensive
        let display = format_signature(name.as_ref(), &required, &default, variadic.as_ref());
        
        Self {
            name,
            display,
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


impl fmt::Display for Signature {
    // lots of allocations...
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(&self.display)
    }
}


fn format_signature(name: Option<&StringSymbol>, required: &[Parameter], default: &[Parameter], variadic: Option<&Parameter>) -> String {
    STRING_TABLE.with(|string_table| {
        let string_table = string_table.borrow();
        
        let name = name
            .map(|name| string_table.resolve(&name));
        
        let required_names = required.iter()
            .map(|param| string_table.resolve(&param.name))
            .collect::<Vec<&str>>()
            .join(", ");
            
        let default_names = default.iter()
            .map(|param| string_table.resolve(&param.name))
            .map(|name| format!("{} = ...", name))
            .collect::<Vec<String>>()
            .join(", ");
        
        let variadic_name = variadic
            .map(|param| string_table.resolve(&param.name))
            .map(|name| format!("{}...", name));
        
        let parameters;
        if let Some(variadic) = variadic_name {
            parameters = [required_names, default_names, variadic].join(", ");
        } else {
            parameters = [required_names, default_names].join(", ");
        }
        
        format!("fun {}({})", name.unwrap_or(""), parameters)
    })
}