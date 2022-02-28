mod ops;

use ops::*;

use crate::parser::expr::{Expr, ExprVariant};
use crate::parser::primary::{Primary, Atom};
use crate::runtime::variant::Variant;
use crate::runtime::operator::{Operator, Unary, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::interpreter::runtime::Scope;
use crate::interpreter::errors::EvalResult;


pub fn eval(local: &Scope<'_>, expr: &Expr) -> EvalResult<Variant> {
    EvalContext::new(local, expr).eval()
}


// tracks the local scope and the innermost Expr
pub struct EvalContext<'r> {
    local: &'r Scope<'r>,
    expr: &'r Expr,
}

impl<'r> EvalContext<'r> {
    pub fn new(local: &'r Scope, expr: &'r Expr) -> Self {
        EvalContext { local, expr }
    }
    
    pub fn eval(&self) -> EvalResult<Variant> {
        self.eval_inner_expr(self.expr.variant())
    }
    
    fn eval_inner_expr(&self, expr: &ExprVariant) -> EvalResult<Variant> {
        match expr {
            ExprVariant::Primary(primary) => self.eval_primary(primary),
            
            ExprVariant::UnaryOp(op, expr) => self.eval_unary_op((*op).into(), expr),
            ExprVariant::BinaryOp(op, lhs, rhs) => self.eval_binary_op((*op).into(), lhs, rhs),
            
            ExprVariant::Assignment(assignment) => unimplemented!(),
            ExprVariant::Tuple(expr_list) => unimplemented!(),
            ExprVariant::ObjectCtor(ctor) => unimplemented!(),
        }
    }

    fn eval_primary(&self, primary: &Primary) -> EvalResult<Variant> {
        let mut value = self.eval_atom(primary.atom())?;
        
        for item in primary.iter_path() {
            unimplemented!();
        }
        
        Ok(value)
    }

    fn eval_atom(&self, atom: &Atom) -> EvalResult<Variant> {
        let value = match atom {
            Atom::Nil => Variant::Nil,
            Atom::EmptyTuple => Variant::EmptyTuple,
            Atom::BooleanLiteral(value) => Variant::Boolean(*value),
            Atom::IntegerLiteral(value) => Variant::Integer(*value),
            Atom::FloatLiteral(value) => Variant::Float(*value),
            Atom::StringLiteral(value) => Variant::InternStr(*value),
            
            Atom::Identifier(name) => unimplemented!(),
            Atom::GlobalIdentifier(name) => unimplemented!(),
            
            Atom::Group(expr) => self.eval_inner_expr(expr)?,
        };
        Ok(value)
    }
    
    fn eval_unary_op(&self, op: Unary, expr: &ExprVariant) -> EvalResult<Variant> {
        let operand = self.eval_inner_expr(expr)?;
        
        match op {
            Unary::Neg => eval_neg(&operand),
            Unary::Pos => eval_pos(&operand),
            Unary::Inv => eval_inv(&operand),
            Unary::Not => eval_not(&operand),
        }
    }
    
    fn eval_binary_op(&self, op: Operator, lhs: &ExprVariant, rhs: &ExprVariant) -> EvalResult<Variant> {
        if let Operator::Logical(logic) = op {
            return self.eval_short_circuit_logic(logic, lhs, rhs);
        }
        
        let lhs_value = self.eval_inner_expr(lhs)?;
        let rhs_value = self.eval_inner_expr(rhs)?;
        
        eval_binary(op, &lhs_value, &rhs_value)
    }
    
    fn eval_short_circuit_logic(&self, op: Logical, lhs: &ExprVariant, rhs: &ExprVariant) -> EvalResult<Variant> {
        let lhs_value = self.eval_inner_expr(lhs)?;
        
        let cond = match op {
            Logical::And => !lhs_value.truth_value(),
            Logical::Or => lhs_value.truth_value(),
        };
        
        if cond {
            Ok(lhs_value)
        } else {
            self.eval_inner_expr(rhs)
        }
    }
    
}

