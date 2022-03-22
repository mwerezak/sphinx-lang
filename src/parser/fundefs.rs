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
    pub name: Option<InternSymbol>,
    pub required: Box<[ParamDef]>,
    pub default: Box<[DefaultDef]>,
    pub variadic: Option<ParamDef>,
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
