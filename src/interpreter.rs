pub mod eval;
pub mod exec;
// pub mod errors;


use crate::parser::expr::ExprVariant;
use crate::parser::stmt::StmtVariant;
use crate::runtime::{Environment, Variant};
use crate::runtime::errors::{EvalResult, ExecResult};
use crate::interpreter::eval::EvalContext;
use crate::interpreter::exec::ExecContext;


pub fn eval<'a, 'r>(env: &'a mut Environment<'r>, expr: &ExprVariant) -> EvalResult<Variant> {
    let mut ctx = EvalContext::from(env);
    ctx.eval(&expr)
}

pub fn exec<'a, 'r>(env: &'a mut Environment<'r>, stmt: &StmtVariant) -> ExecResult<()> {
    let mut ctx = ExecContext::from(env);
    ctx.exec(&stmt)
}