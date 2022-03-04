pub mod eval;
// pub mod errors;


use crate::parser::expr::ExprVariant;
use crate::runtime::{Environment, Variant};
use crate::runtime::errors::EvalResult;
use crate::interpreter::eval::EvalContext;


// A tree-walking interpreter
pub struct Interpreter<'r> {
    env: &'r mut Environment<'r>
}

impl<'r> Interpreter<'r> {
    pub fn new(env: &'r mut Environment<'r>) -> Self {
        Interpreter { env }
    }
    
    // need to use 'a here to ensure that ctx is dropped when the method returns
    pub fn eval<'a>(&'a mut self, expr: &ExprVariant) -> EvalResult<Variant> {
        let mut ctx = EvalContext::new(self.env);
        ctx.eval(&expr)
    }
}