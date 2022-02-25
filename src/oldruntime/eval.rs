#![allow(unused_variables, unused_mut)]

use crate::parser::expr::Expr;
use crate::parser::primary::{Primary, Atom, AccessItem};
use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::runtime::{RuntimeContext, Variant};
use crate::runtime::types::{RuntimeType, CallSlot};
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
        unimplemented!()
        // let opval = self.eval(operand)?;
        // let rtype = opval.rtype(self.runtime.as_ref());
        
        // let result = match op {
        //     UnaryOp::Neg => rtype.slots().call_neg(opval), 
        //     UnaryOp::Pos => unimplemented!(), 
        //     UnaryOp::Inv => unimplemented!(), 
        //     UnaryOp::Not => unimplemented!(),
        // };
        
        // if result.is_none() {
        //     Err(RuntimeError::new(ErrorKind::UnsupportedUnaryOperand(rtype.type_id())))
        // } else {
        //     result.unwrap()
        // }
    }

    fn apply_binary_op(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> RuntimeResult<Variant> {
        // let lhs_val = self.eval(rhs)?;
        // let rhs_val = self.eval(lhs)?;
        
        // let lhs_type = lhs_val.rtype(self.runtime.as_ref());
        // let rhs_type = rhs_val.rtype(self.runtime.as_ref());
        
        // let call_binary_op = |call, rcall| {
        //     let value = lhs_type.slots().call_binary_op(call, lhs_val, rhs_val)?;
        //     if value.is_some() || lhs_type == rhs_type {
        //         value
        //     } else {
        //         rhs_type.slots().call_binary_op(rcall, rhs_val, lhs_val)?
        //     }
        // };
        
        match op {
            BinaryOp::Mul    => self.eval_binary_op(op, lhs, rhs),
            BinaryOp::Div    => self.eval_binary_op(op, lhs, rhs),
            BinaryOp::Mod    => self.eval_binary_op(op, lhs, rhs),
            BinaryOp::Add    => self.eval_binary_op(op, lhs, rhs),
            BinaryOp::Sub    => self.eval_binary_op(op, lhs, rhs),
            BinaryOp::LShift => self.eval_binary_op(op, lhs, rhs),
            BinaryOp::RShift => self.eval_binary_op(op, lhs, rhs),
            BinaryOp::BitAnd => self.eval_binary_op(op, lhs, rhs),
            BinaryOp::BitXor => self.eval_binary_op(op, lhs, rhs),
            BinaryOp::BitOr  => self.eval_binary_op(op, lhs, rhs),
            BinaryOp::LT     => unimplemented!(),
            BinaryOp::GE     => unimplemented!(),
            BinaryOp::LE     => unimplemented!(),
            BinaryOp::GT     => unimplemented!(),
            BinaryOp::EQ     => unimplemented!(),
            BinaryOp::NE     => unimplemented!(),
            BinaryOp::And    => unimplemented!(),
            BinaryOp::Or     => unimplemented!(),
        }
    }
    
    fn eval_binary_op(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> RuntimeResult<Variant> {
        let (call, rcall) =  match op {
            BinaryOp::Mul    => (CallSlot::Mul, CallSlot::RMul),
            BinaryOp::Div    => (CallSlot::Div, CallSlot::RDiv),
            BinaryOp::Mod    => (CallSlot::Mod, CallSlot::RMod),
            BinaryOp::Add    => (CallSlot::Add, CallSlot::RAdd),
            BinaryOp::Sub    => (CallSlot::Sub, CallSlot::RSub),
            BinaryOp::LShift => (CallSlot::Shl, CallSlot::RShl),
            BinaryOp::RShift => (CallSlot::Shr, CallSlot::RShr),
            BinaryOp::BitAnd => (CallSlot::And, CallSlot::RAnd),
            BinaryOp::BitXor => (CallSlot::Xor, CallSlot::RXor),
            BinaryOp::BitOr  => (CallSlot::Or,  CallSlot::ROr),
            _                => panic!(),
        };
        
        let lhs_val = self.eval(rhs)?;
        let rhs_val = self.eval(lhs)?;
        
        let lhs_type = lhs_val.rtype(self.runtime.as_ref());
        let rhs_type = rhs_val.rtype(self.runtime.as_ref());
        
        let mut result = lhs_type.slots().call_binary_op(call, lhs_val, rhs_val)?;
        if let Some(value) = result {
            return Ok(value);
        }
        
        if lhs_type != rhs_type {
            result = rhs_type.slots().call_binary_op(rcall, rhs_val, lhs_val)?;
        }
        
        if let Some(value) = result {
            Ok(value)
        } else {
            Err(RuntimeError::new(ErrorKind::UnsupportedBinaryOperand(op, lhs_type.type_id(), rhs_type.type_id())))
        }
    }
    
    fn eval_comparison_le(&mut self, lhs: &Expr, rhs: &Expr) -> RuntimeResult<bool> {
        let lhs_val = self.eval(rhs)?;
        let rhs_val = self.eval(lhs)?;
        
        let lhs_type = lhs_val.rtype(self.runtime.as_ref());
        let rhs_type = rhs_val.rtype(self.runtime.as_ref());
        
        let mut result = lhs_type.slots().call_lt(lhs_val, rhs_val)?.map(|value| value.truth_value());
        if let Some(value) = result {
            return Ok(value);
        }
        
        if lhs_type != rhs_type {
            result = rhs_type.slots().call_le(rhs_val, lhs_val)?.map(|value| !value.truth_value());
        }
        
        if let Some(value) = result {
            Ok(value)
        } else {
            Err(RuntimeError::new(ErrorKind::UnsupportedBinaryOperand(BinaryOp::LE, lhs_type.type_id(), rhs_type.type_id())))
        }
    }
    
    
    
    // fn call_comparison_op(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> RuntimeResult<Variant> {
    //     // let lhs_val = self.eval(rhs)?;
    //     // let rhs_val = self.eval(lhs)?;
        
    //     // let lhs_type = lhs_val.rtype(self.runtime.as_ref());
    //     // let rhs_type = rhs_val.rtype(self.runtime.as_ref());
        
    //     // let value =  match op {
    //     //     BinaryOp::LT => {
    //     //         let result = lhs_type.slots().call_lt(lhs_val, rhs_val)?;
    //     //         if let Some(value) = result {
    //     //             value.truth_value()
    //     //         } else if lhs_type != rhs_type {
    //     //             let result = rhs_type.slots().call_le(rhs_val, lhs_val)?;
    //     //             if let Some(value) = result {
    //     //                 !value.truth_value()
    //     //             } else {
    //     //                 return Err(RuntimeError::new(ErrorKind::UnsupportedBinaryOperand(op, lhs_type.type_id(), rhs_type.type_id())));
    //     //             }
    //     //         }
    //     //     },
    //     //     BinaryOp::GE => {
    //     //         unimplemented!()
    //     //     },
    //     //     BinaryOp::LE => {
    //     //         unimplemented!()
    //     //     },
    //     //     BinaryOp::GT => {
    //     //         unimplemented!()
    //     //     },
    //     //     BinaryOp::EQ => {
    //     //         unimplemented!()
    //     //     },
    //     //     BinaryOp::NE => {
    //     //         unimplemented!()
    //     //     },
    //     //     _ => panic!(),
    //     unimplemented!()
    // }
    
    // fn eva
}

