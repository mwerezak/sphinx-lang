use crate::language;
use crate::lexer::LexerBuilder;
use crate::runtime::data::StringInterner;

pub struct Runtime {
    pub string_table: StringInterner,
    pub lexer_factory: LexerBuilder,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            string_table: StringInterner::new(),
            lexer_factory: language::create_default_lexer_rules(),
        }
    }
}

use crate::parser::expr::ExprVariant;
use crate::runtime::Variant;
use crate::runtime::errors::EvalResult;
use crate::interpreter::eval::EvalContext;

pub struct Environment<'r> {
    pub runtime: &'r mut Runtime,
}

impl<'r> Environment<'r> {
    // need to use 'a here to ensure that ctx is dropped when the method returns
    pub fn eval<'a>(&'a mut self, expr: &ExprVariant) -> EvalResult<Variant> {
        let mut ctx = EvalContext::new(self);
        ctx.eval(&expr)
    }
}