use std::fmt;
use crate::parser::lvalue::DeclType;
use crate::codegen::{ChunkID, ConstID};
use crate::runtime::Variant;
use crate::runtime::module::{ModuleID, Access};
use crate::runtime::types::Call;
use crate::runtime::strings::{StringSymbol, STRING_TABLE};
use crate::runtime::gc::SizeOf;
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


#[derive(Debug)]
pub struct Function {
    signature: Signature,
    module_id: ModuleID,
    chunk_id: ChunkID,
}

impl SizeOf for Function { }

impl Function {
    pub fn new(signature: Signature, module_id: ModuleID, chunk_id :ChunkID) -> Self {
        Self { signature, module_id, chunk_id }
    }
    
    pub fn signature(&self) -> &Signature { &self.signature }
    
    pub fn as_call(&self) -> Call {
        Call::Chunk(self.module_id, self.chunk_id)
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
    
    pub fn check_args(&self, args: &[Variant]) -> ExecResult<()> {
        if args.len() < self.required.len() {
            return Err(ErrorKind::MissingArguments(args.len(), self.clone()).into())
        }
        
        if matches!(self.max_arity(), Some(max_arity) if args.len() > max_arity) {
            return Err(ErrorKind::TooManyArguments(args.len(), self.clone()).into())
        }
        
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Parameter {
    name: StringSymbol,
    decl: DeclType,
}

impl Parameter {
    pub fn new(name: StringSymbol, decl: DeclType) -> Self {
        Self { name, decl }
    }
    
    pub fn name(&self) -> &StringSymbol { &self.name }
    pub fn decl(&self) -> &DeclType { &self.decl }
}


impl fmt::Display for Signature {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(&self.display)
    }
}

fn format_signature(name: Option<&StringSymbol>, required: &[Parameter], default: &[Parameter], variadic: Option<&Parameter>) -> String {
    STRING_TABLE.with(|string_table| {
        let string_table = string_table.borrow();
        
        let name = name
            .map(|name| string_table.resolve(name));
        
        let mut parameters = Vec::new();
        
        let required_names = required.iter()
            .map(|param| string_table.resolve(&param.name).to_string());
        parameters.extend(required_names);
            
        let default_names = default.iter()
            .map(|param| string_table.resolve(&param.name))
            .map(|name| format!("{} = ...", name));
        parameters.extend(default_names);
        
        let variadic_name = variadic
            .map(|param| string_table.resolve(&param.name))
            .map(|name| format!("{}...", name));
        parameters.extend(variadic_name);
        
        format!("fun {}({})", name.unwrap_or(""), parameters.join(", "))
    })
}