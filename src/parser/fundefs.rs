use crate::runtime::strings::InternSymbol;
use crate::parser::lvalue::DeclType;
use crate::parser::expr::Expr;
use crate::parser::stmt::{StmtMeta, StmtList};

// Function Definitions
#[derive(Debug, Clone)]
pub struct FunctionDef {
    signature: SignatureDef,
    body: StmtList,
}


impl FunctionDef {
    pub fn new(signature: SignatureDef, body: StmtList) -> Self {
        FunctionDef {
            signature, body,
        }
    }
}


#[derive(Debug, Clone)]
pub struct SignatureDef {
    required: Box<[ParamDef]>,
    default: Box<[ParamDef]>,
    variadic: Option<ParamDef>,
}

impl SignatureDef {
    pub fn new(required: Vec<ParamDef>, default: Vec<ParamDef>, variadic: Option<ParamDef>) -> Self {
        SignatureDef {
            required: required.into_boxed_slice(),
            default: default.into_boxed_slice(),
            variadic,
        }
    }
    
    pub fn is_variadic(&self) -> bool { self.variadic.is_some() }
}


#[derive(Debug, Clone)]
pub struct ParamDef {
    name: InternSymbol,
    decl: DeclType,
    default: Option<Box<Expr>>,
}

impl ParamDef {
    pub fn new(name: InternSymbol, decl: DeclType, default: Option<Expr>) -> Self {
        ParamDef {
            name, decl,
            default: default.map(|expr| Box::new(expr)),
        }
    }
}