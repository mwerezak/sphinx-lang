use string_interner::StringInterner;

use crate::lexer::{Lexer, LexerBuilder};
use crate::parser::Parser;
use crate::parser::expr::Expr;
use crate::runtime::data::{Variant, InternStr, StrBackend};
use crate::runtime::eval::EvalContext;
use crate::runtime::errors::RuntimeResult;

// container for data structures necessary for the language runtime
pub struct Runtime {
    interner: StringInterner<StrBackend>,
    lexer_factory: LexerBuilder,
    // globals
    // loaded modules
}

impl Runtime {
    pub fn new(lexer_factory: LexerBuilder) -> Self {
        Runtime { 
            lexer_factory, 
            interner: Default::default(),
        }
    }
    
    pub fn create_parser<'m, 'h, S>(&'h mut self, module: &'m Module, source: S) -> Parser<'m, 'h, Lexer<S>>
    where S: Iterator<Item=char> {
        let lexer = self.lexer_factory.build(source);
        Parser::new(module, &mut self.interner, lexer)
    }
    
    // Interning strings
    
    pub fn get_or_intern_str<S>(&mut self, string: S) -> InternStr where S: AsRef<str> {
        InternStr::from_str(string.as_ref(), &mut self.interner)
    }
    
    // Attempting to resolve an InternStr that was created with a different 
    // Runtime instance may produce unpredictable results or panic.
    pub fn resolve_str(&self, string: InternStr) -> &str {
        self.interner.resolve(string.symbol()).unwrap()
    }
    
}


// runtime execution context
pub struct RuntimeContext<'r> {
    runtime: &'r Runtime,
    // current local scope
}

impl<'r> RuntimeContext<'r> {
    pub fn runtime(&self) -> &Runtime { self.runtime }
    
    // Evaluate expressions
    
    pub fn eval(&'r mut self, expr: &Expr) -> RuntimeResult<Variant> {
        EvalContext::<'r>::new(self).eval(expr)
    }
}


// TODO rename to ModuleInfo or ModuleMetadata or ModuleSource, something like that...
// should hold the information needed to load up the original source and print error messages
#[derive(Debug)]
pub struct Module {
    name: String,
}

// Temporary for development
pub fn placeholder_module(name: &str) -> Module {
    Module { name: name.to_string() }
}

pub fn placeholder_runtime_ctx(runtime: &Runtime) -> RuntimeContext {
    RuntimeContext { runtime }
}