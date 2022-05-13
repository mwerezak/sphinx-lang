use crate::language::{InternSymbol, Access};
use crate::parser::expr::{ExprMeta, ExprBlock};


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

impl SignatureDef {
    pub fn param_count(&self) -> usize {
        self.required.len()
        + self.default.len()
        + usize::from(self.variadic.is_some())
    }
}

#[derive(Debug, Clone)]
pub struct ParamDef {
    pub name: InternSymbol,
    pub mode: Access,
}

#[derive(Debug, Clone)]
pub struct DefaultDef {
    pub name: InternSymbol,
    pub mode: Access,
    pub default: Box<ExprMeta>,
}
