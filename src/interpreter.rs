pub mod eval;
pub mod exec;
// pub mod errors;


use crate::parser::expr::ExprVariant;
use crate::parser::stmt::StmtVariant;
use crate::runtime::{Runtime, Variant};
use crate::runtime::errors::{EvalResult, ExecResult};
use crate::interpreter::eval::EvalContext;
use crate::interpreter::exec::ExecContext;


pub fn eval<'a, 'r>(runtime: &'a mut Runtime<'r>, expr: &ExprVariant) -> EvalResult<Variant> {
    let mut ctx = EvalContext::from(runtime);
    ctx.eval(&expr)
}

pub fn exec<'a, 'r>(runtime: &'a mut Runtime<'r>, stmt: &StmtVariant) -> ExecResult<()> {
    let mut ctx = ExecContext::from(runtime);
    ctx.exec(&stmt)
}