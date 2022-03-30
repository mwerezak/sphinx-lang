use std::fmt;
use crate::parser::lvalue::DeclType;
use crate::codegen::{ChunkID, ConstID};
use crate::runtime::Variant;
use crate::runtime::module::{Module, Access};
use crate::runtime::strings::{StringSymbol, STRING_TABLE};
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


/// Call directive


pub enum Call {
    Chunk(GC<Module>, ChunkID),
    Native(GC<NativeFunction>),
}

pub trait Invoke {
    fn signature(&self) -> &Signature;
    fn as_call(&self) -> Call;
    
    fn invoke(&self, args: &[Variant]) -> ExecResult<Call> {
        self.signature().check_args(args)?;
        Ok(self.as_call())
    }
}


#[derive(Debug)]
pub struct Function {
    signature: Signature,
    module: GC<Module>,
    chunk_id: ChunkID,
}

impl GCTrace for Function { }

impl Function {
    pub fn new(signature: Signature, module: GC<Module>, chunk_id :ChunkID) -> Self {
        Self { signature, module, chunk_id }
    }
}

impl Invoke for Function {
    fn signature(&self) -> &Signature { &self.signature }
    
    fn as_call(&self) -> Call {
        Call::Chunk(self.module, self.chunk_id)
    }
}


pub type NativeFn = fn(self_fun: &NativeFunction, args: &[Variant]) -> ExecResult<Variant>;

pub struct NativeFunction {
    signature: Signature,
    defaults: Option<Box<[Variant]>>,
    func: NativeFn,
}

impl GCTrace for NativeFunction { }

impl NativeFunction {
    pub fn new(signature: Signature, defaults: Option<Box<[Variant]>>, func: NativeFn) -> Self {
        Self { signature, defaults, func }
    }
    
    pub fn signature(&self) -> &Signature { &self.signature }
    
    pub fn defaults(&self) -> &[Variant] {
        match self.defaults.as_ref() {
            Some(defaults) => &*defaults,
            None => &[],
        }
    }
    
    pub fn invoke(&self, args: &[Variant]) -> ExecResult<Variant> {
        (self.func)(self, args)
    }
}

impl Invoke for GC<NativeFunction> {
    fn signature(&self) -> &Signature { &self.signature }
    
    fn as_call(&self) -> Call {
        Call::Native(*self)
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
    pub fn new(name: Option<impl Into<StringSymbol>>, required: Vec<Parameter>, default: Vec<Parameter>, variadic: Option<Parameter>) -> Self {
        let name = name.map(|name| name.into());
        
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
    
    /// Get the length of the argument buffer required by bind_args()
    pub fn arg_len(&self) -> usize {
        self.required.len() + self.default.len()
    }
    
    /// Helper for native functions. Prepares a complete argument buffer by cloning argument values,
    /// Assumes check_args() has already succeeded.
    pub fn bind_args<'a>(&self, args: &'a [Variant], defaults: &'a [Variant], argbuf: &'a mut [Variant]) -> BoundArgs<'a> {
        debug_assert!(args.len() >= self.required.len());
        debug_assert!(argbuf.len() == self.arg_len());
        debug_assert!(defaults.len() == self.default.len());
        
        let mut arg_idx = 0;
        for _ in self.required.iter() {
            argbuf[arg_idx] = args[arg_idx];
            arg_idx += 1;
        }
        
        for (default_idx, _) in self.default.iter().enumerate() {
            if arg_idx < args.len() {
                argbuf[arg_idx] = args[arg_idx];
            } else {
                argbuf[arg_idx] = defaults[default_idx];
            }
            arg_idx += 1;
        }
        
        let varargs;
        if arg_idx > args.len() {
            varargs = &[] as &[Variant];
        } else {
            let (_, rest) = args.split_at(arg_idx);
            varargs = rest;
        }
        
        BoundArgs {
            args: argbuf,
            varargs,
        }
    }
}

pub struct BoundArgs<'a> {
    pub args: &'a [Variant],
    pub varargs: &'a [Variant],
}


#[derive(Clone, Debug)]
pub struct Parameter {
    name: StringSymbol,
    decl: DeclType,
}

impl Parameter {
    pub fn new(name: impl Into<StringSymbol>, decl: DeclType) -> Self {
        Self { name: name.into(), decl }
    }
    
    pub fn new_let(name: impl Into<StringSymbol>) -> Self {
        Self::new(name, DeclType::Immutable)
    }
    
    pub fn new_var(name: impl Into<StringSymbol>) -> Self {
        Self::new(name, DeclType::Mutable)
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
