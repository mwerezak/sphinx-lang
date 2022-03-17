use crate::runtime::strings::InternSymbol;
use crate::parser::assign::DeclType;
use crate::parser::expr::Expr;
use crate::parser::stmt::{StmtMeta, StmtList};

// Function Definitions
#[derive(Debug, Clone)]
pub struct FunctionDef {
    signature: FunSignature,
    body: StmtList,
}


impl FunctionDef {
    pub fn new(signature: FunSignature, body: StmtList) -> Self {
        FunctionDef {
            signature, body,
        }
    }
}


#[derive(Debug, Clone)]
pub struct FunSignature {
    required: Box<[FunParam]>,
    default: Box<[FunParam]>,
    variadic: Option<FunParam>,
}

impl FunSignature {
    pub fn new(required: Vec<FunParam>, default: Vec<FunParam>, variadic: Option<FunParam>) -> Self {
        FunSignature {
            required: required.into_boxed_slice(),
            default: default.into_boxed_slice(),
            variadic,
        }
    }
    
    pub fn min_arity(&self) -> usize { self.required.len() }
    
    pub fn max_arity(&self) -> Option<usize> {
        if self.is_variadic() { None }
        else { Some(self.required.len() + self.default.len()) }
    }
    
    pub fn is_variadic(&self) -> bool { self.variadic.is_some() }
}


#[derive(Debug, Clone)]
pub struct FunParam {
    name: InternSymbol,
    decl: DeclType,
    default: Option<Box<Expr>>,
}

impl FunParam {
    pub fn new(name: InternSymbol, decl: DeclType, default: Option<Expr>) -> Self {
        FunParam {
            name, decl,
            default: default.map(|expr| Box::new(expr)),
        }
    }
}