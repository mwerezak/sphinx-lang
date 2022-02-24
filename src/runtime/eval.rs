use crate::parser::expr::Expr;
use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::runtime::{Runtime, Variant};
use crate::runtime::errors::RuntimeError;

pub fn eval(runtime: &mut Runtime, expr: &Expr) -> Result<Variant, RuntimeError> {
    unimplemented!()
}

fn apply_unary_op(op: UnaryOp, a: &Expr) -> Result<Variant, RuntimeError> {
    unimplemented!()
}

fn apply_binary_op(op: BinaryOp, a: &Expr, b: &Expr) -> Result<Variant, RuntimeError> {
    unimplemented!()
}