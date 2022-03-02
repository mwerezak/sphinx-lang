#![allow(unused_variables)]
#![allow(unused_mut)]

use crate::debug::symbol::DebugSymbol;
use crate::parser::expr::{Expr, ExprVariant};
use crate::parser::primary::{Primary, Atom};
use crate::runtime::Variant;
use crate::runtime::ops::*;
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::errors::EvalResult;
use crate::interpreter::runtime::Scope;


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
            Atom::BooleanLiteral(true) => Variant::BoolTrue,
            Atom::BooleanLiteral(false) => Variant::BoolFalse,
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
        
        let result = match op {
            BinaryOp::Arithmetic(op) => match op {
                Arithmetic::Mul    => eval_mul(&lhs_value, &rhs_value)?,
                Arithmetic::Div    => eval_div(&lhs_value, &rhs_value)?,
                Arithmetic::Mod    => eval_mod(&lhs_value, &rhs_value)?,
                Arithmetic::Add    => eval_add(&lhs_value, &rhs_value)?,
                Arithmetic::Sub    => eval_sub(&lhs_value, &rhs_value)?,
            },
            
            BinaryOp::Bitwise(op) => match op {
                Bitwise::And => eval_and(&lhs_value, &rhs_value),
                Bitwise::Xor => eval_xor(&lhs_value, &rhs_value),
                Bitwise::Or  => eval_or(&lhs_value, &rhs_value),
            },
            
            BinaryOp::Shift(op) => match op {
                Shift::Left => eval_shl(&lhs_value, &rhs_value)?,
                Shift::Right => eval_shr(&lhs_value, &rhs_value)?,
            }
            
            BinaryOp::Comparison(op) => match op {
                Comparison::LT     => eval_lt(&lhs_value, &rhs_value),
                Comparison::GT     => eval_gt(&lhs_value, &rhs_value),
                Comparison::LE     => eval_le(&lhs_value, &rhs_value),
                Comparison::GE     => eval_ge(&lhs_value, &rhs_value),
                
                Comparison::EQ     => Some(eval_eq(&lhs_value, &rhs_value)),
                Comparison::NE     => Some(eval_ne(&lhs_value, &rhs_value)),
            
            }.map(Variant::from),
            
            _ => None,
        };
        
        if let Some(value) = result {
            Ok(value)
        } else {
            Self::eval_binary_from_type(op, &lhs_value, &rhs_value)
        }
    }
    
    // This will probably be replaced once type system is implemented
    fn eval_binary_from_type(_op: BinaryOp, _lhs: &Variant, _rhs: &Variant) -> EvalResult<Variant> {
        // TODO defer to lhs's type metamethods, or rhs's type reflected metamethod
        unimplemented!()
    }
    
}

