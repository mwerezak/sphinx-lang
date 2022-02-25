#![allow(unused_variables, unused_mut)]

use crate::parser::expr::Expr;
use crate::parser::primary::{Primary, Atom, AccessItem};
use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::runtime::{RuntimeContext, Variant};
use crate::runtime::errors::{RuntimeError, RuntimeResult, ErrorKind};

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
            Atom::FloatLiteral(value) => Variant::Float(*value),
            Atom::StringLiteral(intern) => Variant::InternString(*intern),
            
            Atom::Group(ref expr) => self.eval(expr)?,
        };
        Ok(value)
    }
    
    fn apply_unary_op(&mut self, op: &UnaryOp, operand: &Expr) -> RuntimeResult<Variant> {
        let opval = self.eval(operand)?;
        let rtype = opval.rtype(self.runtime.as_ref());
        
        let result = match op {
            UnaryOp::Neg => rtype.slots().call_neg(opval), 
            UnaryOp::Pos => unimplemented!(), 
            UnaryOp::Inv => unimplemented!(), 
            UnaryOp::Not => unimplemented!(),
        };
        
        if result.is_none() {
            Err(RuntimeError::new(ErrorKind::UnsupportedUnaryOperand(rtype.type_id())))
        } else {
            result.unwrap()
        }
    }

    fn apply_binary_op(&mut self, op: &BinaryOp, lhs: &Expr, rhs: &Expr) -> RuntimeResult<Variant> {
        let lhs_val = self.eval(rhs)?;
        let rhs_val = self.eval(lhs)?;
        
        let lhs_type = lhs_val.rtype(self.runtime.as_ref());
        let rhs_type = rhs_val.rtype(self.runtime.as_ref());
        
        let result = match op {
            BinaryOp::Mul    => {
                let value = lhs_type.slots().call_mul(lhs_val, rhs_val);
                if value.is_some() || lhs_type == rhs_type {
                    value
                } else {
                    rhs_type.slots().call_rmul(rhs_val, lhs_val)
                }
            },
            BinaryOp::Div    => {
                let value = lhs_type.slots().call_div(lhs_val, rhs_val);
                if value.is_some() || lhs_type == rhs_type {
                    value
                } else {
                    rhs_type.slots().call_rdiv(rhs_val, lhs_val)
                }
            },
            BinaryOp::Mod    => {
                let value = lhs_type.slots().call_mod(lhs_val, rhs_val);
                if value.is_some() || lhs_type == rhs_type {
                    value
                } else {
                    rhs_type.slots().call_rmod(rhs_val, lhs_val)
                }
            },
            BinaryOp::Add    => {
                let value = lhs_type.slots().call_add(lhs_val, rhs_val);
                if value.is_some() || lhs_type == rhs_type {
                    value
                } else {
                    rhs_type.slots().call_radd(rhs_val, lhs_val)
                }
            },
            BinaryOp::Sub    => {
                let value = lhs_type.slots().call_sub(lhs_val, rhs_val);
                if value.is_some() || lhs_type == rhs_type {
                    value
                } else {
                    rhs_type.slots().call_rsub(rhs_val, lhs_val)
                }
            },
            BinaryOp::LShift => {
                let value = lhs_type.slots().call_shl(lhs_val, rhs_val);
                if value.is_some() || lhs_type == rhs_type {
                    value
                } else {
                    rhs_type.slots().call_rshl(rhs_val, lhs_val)
                }
            },
            BinaryOp::RShift => {
                let value = lhs_type.slots().call_shr(lhs_val, rhs_val);
                if value.is_some() || lhs_type == rhs_type {
                    value
                } else {
                    rhs_type.slots().call_rshr(rhs_val, lhs_val)
                }
            },
            BinaryOp::BitAnd => {
                let value = lhs_type.slots().call_and(lhs_val, rhs_val);
                if value.is_some() || lhs_type == rhs_type {
                    value
                } else {
                    rhs_type.slots().call_rand(rhs_val, lhs_val)
                }
            },
            BinaryOp::BitXor => {
                let value = lhs_type.slots().call_xor(lhs_val, rhs_val);
                if value.is_some() || lhs_type == rhs_type {
                    value
                } else {
                    rhs_type.slots().call_rxor(rhs_val, lhs_val)
                }
            },
            BinaryOp::BitOr  => {
                let value = lhs_type.slots().call_or(lhs_val, rhs_val);
                if value.is_some() || lhs_type == rhs_type {
                    value
                } else {
                    rhs_type.slots().call_ror(rhs_val, lhs_val)
                }
            },
            BisubyOp::LT     => unimplemented!(),
            BinaryOp::GT     => unimplemented!(),
            BinaryOp::LE     => unimplemented!(),
            BinaryOp::GE     => unimplemented!(),
            BinaryOp::EQ     => unimplemented!(),
            BinaryOp::NE     => unimplemented!(),
            BinaryOp::And    => unimplemented!(),
            BinaryOp::Or     => unimplemented!(),
        };
        
        if result.is_none() {
            Err(RuntimeError::new(ErrorKind::UnsupportedBinaryOperand(lhs_type.type_id(), rhs_type.type_id())))
        } else {
            result.unwrap()
        }
    }
}


macro_rules! binary_op_helper {
    ( $call_slot:tt, $call_rslot:tt ) => {
        
        
    };
}