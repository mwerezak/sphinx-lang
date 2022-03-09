pub mod eval;
pub mod exec;

pub use eval::{eval_expr, eval_expr_variant};
pub use exec::ExecContext;

// pub mod errors;


// use crate::parser::expr::Expr;
// use crate::parser::stmt::Stmt;
// use crate::runtime::{Runtime, Variant};
// use crate::runtime::errors::ExecResult;
// use crate::interpreter::eval::EvalContext;
// use crate::interpreter::exec::ExecContext;


// pub fn eval<'a, 'r>(runtime: &'a mut Runtime<'r>, expr: &Expr) -> ExecResult<Variant> {
//     let mut ctx = EvalContext::from(runtime);
//     ctx.eval(&expr)
// }

// pub fn exec<'a, 'r>(runtime: &'a mut Runtime<'r>, stmt: &Stmt) -> ExecResult<()> {
//     let mut ctx = ExecContext::from(runtime);
//     ctx.exec(&stmt)
// }