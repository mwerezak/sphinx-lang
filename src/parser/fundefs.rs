use crate::runtime::strings::InternSymbol;
use crate::parser::lvalue::DeclType;
use crate::parser::expr::ExprMeta;
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
    
    pub fn signature(&self) -> &SignatureDef { &self.signature }
    
    pub fn body(&self) -> &StmtList { &self.body }
}


#[derive(Debug, Clone)]
pub struct SignatureDef {
    pub required: Box<[RequiredDef]>,
    pub default: Box<[DefaultDef]>,
    pub variadic: Option<VariadicDef>,
}

impl SignatureDef {
    pub fn new(required: Vec<RequiredDef>, default: Vec<DefaultDef>, variadic: Option<VariadicDef>) -> Self {
        SignatureDef {
            required: required.into_boxed_slice(),
            default: default.into_boxed_slice(),
            variadic,
        }
    }
    
    pub fn is_variadic(&self) -> bool { self.variadic.is_some() }
}


#[derive(Debug, Clone)]
pub struct RequiredDef {
    pub name: InternSymbol,
    pub decl: DeclType,
}

#[derive(Debug, Clone)]
pub struct DefaultDef {
    pub name: InternSymbol,
    pub decl: DeclType,
    pub default: Box<ExprMeta>,
}

#[derive(Debug, Clone)]
pub struct VariadicDef {
    pub name: InternSymbol,
    pub decl: DeclType,
    pub default: Option<Box<ExprMeta>>,
}
