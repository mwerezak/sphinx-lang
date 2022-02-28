use crate::language::{IntType, FloatType};
use crate::parser::operator::{UnaryOp, BinaryOp};
use crate::runtime::variant::{Variant, Primitive};
use crate::runtime::errors::{EvalResult, EvalErrorKind};

// Unary Operators

pub fn eval_neg(operand: &Variant) -> EvalResult<Variant> {
    let value = match operand {
        Variant::Integer(value) => Variant::Integer(-value),
        Variant::Float(value) => Variant::Float(-value),
        _ => return eval_unary_other(UnaryOp::Neg, operand),
    };
    Ok(value)
}

pub fn eval_pos(operand: &Variant) -> EvalResult<Variant> {
    if operand.pri_type().is_numeric() {
        return Ok(*operand);
    }
    
    eval_unary_other(UnaryOp::Pos, operand)
}

pub fn eval_inv(operand: &Variant) -> EvalResult<Variant> {
    if let Variant::Integer(value) = operand {
        return Ok(Variant::Integer(!value))
    }
    
    eval_unary_other(UnaryOp::Inv, operand)
}

pub fn eval_not(operand: &Variant) -> EvalResult<Variant> {
    return Ok(Variant::Boolean(!operand.truth_value()))
}


fn eval_unary_other(op: UnaryOp, operand: &Variant) -> EvalResult<Variant> {
    // TODO defer to the operand's type's metamethods
    unimplemented!()
}

// Binary Operators

pub fn eval_binary(op: BinaryOp, lhs: &Variant, rhs: &Variant) -> EvalResult<Variant> {
    // try a numeric evaluation short-circuit
    let result = match op {
        BinaryOp::Mul    => eval_mul(&lhs, &rhs)?,
        BinaryOp::Div    => eval_div(&lhs, &rhs)?,
        BinaryOp::Mod    => eval_mod(&lhs, &rhs)?,
        BinaryOp::Add    => eval_add(&lhs, &rhs)?,
        BinaryOp::Sub    => eval_sub(&lhs, &rhs)?,
        BinaryOp::LShift => eval_shl(&lhs, &rhs)?,
        BinaryOp::RShift => eval_shr(&lhs, &rhs)?,
        BinaryOp::BitAnd => eval_and(&lhs, &rhs)?,
        BinaryOp::BitXor => eval_xor(&lhs, &rhs)?,
        BinaryOp::BitOr  => eval_or(&lhs, &rhs)?,
        BinaryOp::LT     => unimplemented!(),
        BinaryOp::GT     => unimplemented!(),
        BinaryOp::LE     => unimplemented!(),
        BinaryOp::GE     => unimplemented!(),
        BinaryOp::EQ     => unimplemented!(),
        BinaryOp::NE     => unimplemented!(),
        BinaryOp::And    => unimplemented!(),
        BinaryOp::Or     => unimplemented!(),
    };
    
    if let Some(value) = result {
        Ok(value)
    } else {
        eval_binary_other(op, lhs, rhs)
    }
}

pub fn eval_binary_other(op: BinaryOp, lhs: &Variant, rhs: &Variant) -> EvalResult<Variant> {
    // TODO defer to lhs's type metamethods, or rhs's type reflected metamethod
    unimplemented!()
}


// Numeric Operations

// lots of boilerplate
macro_rules! eval_binary_numeric {
    
    // Arithmetic
    ($name:tt, $int_name:tt, $float_name:tt) => {
        
        fn $name (lhs: &Variant, rhs: &Variant) -> EvalResult<Option<Variant>> {
            let value = match (lhs.pri_type(), rhs.pri_type()) {
                (Primitive::Integer, Primitive::Integer) => $int_name (lhs.int_value()?, rhs.int_value()?),
                (lt, rt) if lt.is_numeric() && rt.is_numeric() => $float_name (lhs.float_value()?, rhs.float_value()?),
                _ => return Ok(None),
            };
            Ok(Some(value))
        }
        
    };
    
    // Bitwise
    
    ($name:tt, $int_name:tt) => {
        
        fn $name (lhs: &Variant, rhs: &Variant) -> EvalResult<Option<Variant>> {
            if lhs.pri_type() == Primitive::Integer && rhs.pri_type() == Primitive::Integer {
                Ok(Some($int_name (lhs.int_value()?, rhs.int_value()?)))
            } else {
                Ok(None)
            }
        }
        
    };
}

// Arithmetic

eval_binary_numeric!(eval_mul, int_mul, float_mul);
fn int_mul(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs * rhs) }
fn float_mul(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs * rhs) }

eval_binary_numeric!(eval_div, int_div, float_div);
fn int_div(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs / rhs) }
fn float_div(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs / rhs) }

eval_binary_numeric!(eval_mod, int_mod, float_mod);
fn int_mod(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs % rhs) }
fn float_mod(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs % rhs) }

eval_binary_numeric!(eval_add, int_add, float_add);
fn int_add(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs + rhs) }
fn float_add(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs + rhs) }

eval_binary_numeric!(eval_sub, int_sub, float_sub);
fn int_sub(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs - rhs) }
fn float_sub(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs - rhs) }

// Bitwise

eval_binary_numeric!(eval_shl, int_shl);
fn int_shl(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs << rhs) }

eval_binary_numeric!(eval_shr, int_shr);
fn int_shr(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs >> rhs) }

eval_binary_numeric!(eval_and, int_and);
fn int_and(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs & rhs) }

eval_binary_numeric!(eval_xor, int_xor);
fn int_xor(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs ^ rhs) }

eval_binary_numeric!(eval_or, int_or);
fn int_or(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs | rhs) }