use crate::parser::expr::Expr;
use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::runtime::{RuntimeContext, Variant};
use crate::runtime::errors::{RuntimeError, RuntimeResult};

pub struct EvalContext<'r> {
    runtime: &'r mut RuntimeContext<'r>
}

impl EvalContext<'_> {
    pub fn eval(&mut self, expr: &Expr) -> RuntimeResult<Variant> {
        match expr {
            Expr::Primary(ref primary) => unimplemented!(),

            Expr::UnaryOp(op, ref operand) => self.apply_unary_op(op, operand),

            Expr::BinaryOp(op, ref lhs, ref rhs) => self.apply_binary_op(op, lhs, rhs),

            Expr::Assignment(ref assignment) => unimplemented!(),
            
            Expr::Tuple(ref items) => unimplemented!(),
            
            Expr::ObjectCtor(..) => unimplemented!(),
        }
    }
    
    fn apply_unary_op(&mut self, op: &UnaryOp, operand: &Expr) -> RuntimeResult<Variant> {
        unimplemented!()
    }

    fn apply_binary_op(&mut self, op: &BinaryOp, lhs: &Expr, rhs: &Expr) -> RuntimeResult<Variant> {
        unimplemented!()
    }
}


