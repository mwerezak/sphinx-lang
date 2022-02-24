#![allow(unused_variables, unused_mut)]

use crate::parser::expr::Expr;
use crate::parser::primary::{Primary, Atom, AccessItem};
use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::runtime::{RuntimeContext, Variant};
use crate::runtime::errors::{RuntimeError, RuntimeResult};

pub struct EvalContext<'r> {
    runtime: &'r mut RuntimeContext<'r>
}

impl<'r> EvalContext<'r> {
    pub fn new(runtime: &'r mut RuntimeContext<'r>) -> Self {
        EvalContext { runtime }
    }
    
    pub fn eval(&mut self, expr: &Expr) -> RuntimeResult<Variant> {
        match expr {
            Expr::Primary(ref primary) => self.eval_primary(primary),

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
    
    fn eval_primary(&mut self, primary: &Primary) -> RuntimeResult<Variant> {
        let mut value = self.eval_atom(primary.atom())?;
        
        for access in primary.iter_path() {
            match access {
                AccessItem::Member(intern) => unimplemented!(),
                AccessItem::Index(ref expr) => unimplemented!(),
                AccessItem::Invoke(..) => unimplemented!(),
                AccessItem::Construct(ctor) => unimplemented!(),
            }
        }
        
        Ok(value)
    }
    
    fn eval_atom(&mut self, atom: &Atom) -> RuntimeResult<Variant> {
        let value = match atom {
            Atom::Identifier(intern) => unimplemented!(),
            Atom::GlobalIdentifier(intern) => unimplemented!(),
            
            Atom::Nil => Variant::Nil,
            Atom::EmptyTuple => Variant::Empty,
            
            Atom::BooleanLiteral(value) => Variant::Boolean(*value),
            Atom::IntegerLiteral(value) => Variant::Integer(*value),
            Atom::FloatLiteral(value) => Variant::Real(*value),
            Atom::StringLiteral(intern) => Variant::InternString(*intern),
            
            Atom::Group(ref expr) => self.eval(expr)?,
        };
        Ok(value)
    }
}


