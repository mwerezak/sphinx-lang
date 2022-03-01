mod ops;

use ops::*;

use crate::debug::symbol::DebugSymbol;
use crate::parser::expr::{Expr, ExprVariant};
use crate::parser::primary::{Primary, Atom};
use crate::runtime::variant::Variant;
use crate::runtime::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::interpreter::runtime::Scope;
use crate::interpreter::errors::EvalResult;


pub fn eval_expr(local: &Scope<'_>, expr: &ExprVariant, debug: Option<&DebugSymbol>) -> EvalResult<Variant> {
    EvalContext::new(local, debug).eval(expr)
}


// tracks the local scope and the innermost Expr
pub struct EvalContext<'r> {
    local: &'r Scope<'r>,
    debug: Option<&'r DebugSymbol>,
}

impl<'r> EvalContext<'r> {
    pub fn new(local: &'r Scope, debug: Option<&'r DebugSymbol>) -> Self {
        EvalContext { local, debug }
    }
    
    pub fn eval(&self, expr: &ExprVariant) -> EvalResult<Variant> {
        self.eval_inner_expr(expr)
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
            Atom::UpvalIdentifier(name) => unimplemented!(),
            Atom::GlobalIdentifier(name) => unimplemented!(),
            
            Atom::Group(expr) => self.eval_inner_expr(expr)?,
        };
        Ok(value)
    }
    
    fn eval_unary_op(&self, op: UnaryOp, expr: &ExprVariant) -> EvalResult<Variant> {
        let operand = self.eval_inner_expr(expr)?;
        
        match op {
            UnaryOp::Neg => eval_neg(&operand),
            UnaryOp::Pos => eval_pos(&operand),
            UnaryOp::Inv => eval_inv(&operand),
            UnaryOp::Not => eval_not(&operand),
        }
    }
    
    fn eval_binary_op(&self, op: BinaryOp, lhs: &ExprVariant, rhs: &ExprVariant) -> EvalResult<Variant> {
        if let BinaryOp::Logical(logic) = op {
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

