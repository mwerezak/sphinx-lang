use crate::runtime::strings::InternSymbol;
use crate::parser::lvalue::DeclType;
use crate::parser::expr::{ExprMeta, ExprBlock};
use crate::parser::stmt::StmtMeta;

// Function Definitions
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub signature: SignatureDef,
    pub body: Box<ExprBlock>,
}


#[derive(Debug, Clone)]
pub struct SignatureDef {
    pub required: Box<[ParamDef]>,
    pub default: Box<[DefaultDef]>,
    pub variadic: Option<ParamDef>,
}

impl SignatureDef {
    pub fn new(required: Vec<ParamDef>, default: Vec<DefaultDef>, variadic: Option<ParamDef>) -> Self {
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
    pub name: InternSymbol,
    pub decl: DeclType,
}

#[derive(Debug, Clone)]
pub struct DefaultDef {
    pub name: InternSymbol,
    pub decl: DeclType,
    pub default: Box<ExprMeta>,
}
