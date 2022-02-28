use crate::language::{IntType, FloatType};
use crate::parser::operator::{UnaryOp, BinaryOp};
use crate::runtime::variant::Variant;
use crate::interpreter::errors::{EvalResult, EvalErrorKind};

pub fn is_arithmetic_primitive(value: &Variant) -> bool {
    matches!(value, Variant::Integer(..) | Variant::Float(..))
}

pub fn is_bitwise_primitive(value: &Variant) -> bool {
    matches!(value, Variant::Boolean(..) | Variant::Integer(..))
}

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
    let value = match operand {
        // No-op for arithmetic primitives
        Variant::Integer(value) => Variant::Integer(*value),
        Variant::Float(value) => Variant::Float(*value),
        _ => return eval_unary_other(UnaryOp::Pos, operand),
    };
    Ok(value)
}

pub fn eval_inv(operand: &Variant) -> EvalResult<Variant> {
    let value = match operand {
        Variant::Boolean(value) => Variant::Boolean(!value),
        Variant::Integer(value) => Variant::Integer(!value),
        _ => return eval_unary_other(UnaryOp::Inv, operand),
    };
    Ok(value)
}

pub fn eval_not(operand: &Variant) -> EvalResult<Variant> {
    return Ok(Variant::Boolean(!operand.truth_value()))
}

fn eval_unary_other(_op: UnaryOp, _operand: &Variant) -> EvalResult<Variant> {
    // TODO defer to the operand's type's metamethods
    unimplemented!()
}

// Binary Operators

pub fn eval_binary(op: BinaryOp, lhs: &Variant, rhs: &Variant) -> EvalResult<Variant> {
    // try a numeric short-circuit 
    let result = match op {
        BinaryOp::Mul    => eval_mul(&lhs, &rhs),
        BinaryOp::Div    => eval_div(&lhs, &rhs),
        BinaryOp::Mod    => eval_mod(&lhs, &rhs),
        BinaryOp::Add    => eval_add(&lhs, &rhs),
        BinaryOp::Sub    => eval_sub(&lhs, &rhs),
        BinaryOp::LShift => eval_shl(&lhs, &rhs),
        BinaryOp::RShift => eval_shr(&lhs, &rhs),
        BinaryOp::BitAnd => eval_and(&lhs, &rhs),
        BinaryOp::BitXor => eval_xor(&lhs, &rhs),
        BinaryOp::BitOr  => eval_or(&lhs, &rhs),
        BinaryOp::LT     => eval_lt(&lhs, &rhs),
        BinaryOp::GT     => eval_gt(&lhs, &rhs),
        BinaryOp::LE     => eval_le(&lhs, &rhs),
        BinaryOp::GE     => eval_ge(&lhs, &rhs),
        BinaryOp::EQ     => eval_eq(&lhs, &rhs),
        BinaryOp::NE     => eval_ne(&lhs, &rhs),
        _ => None,
    };
    
    if let Some(value) = result {
        Ok(value)
    } else {
        eval_binary_other(op, lhs, rhs)
    }
}

fn eval_binary_other(_op: BinaryOp, _lhs: &Variant, _rhs: &Variant) -> EvalResult<Variant> {
    // TODO defer to lhs's type metamethods, or rhs's type reflected metamethod
    unimplemented!()
}


// Numeric Operations

// These are used to short-circuit the general metamethod lookup path to evaluate of binary operators
// They always succeed (due to the way the numeric coercion rules are set up), and hence don't use EvalResult. 
// Instead, they produce an Option and return None if the operands aren't the right type to short-circuit

// lots of boilerplate
macro_rules! eval_binary_arithmetic {
    ($name:tt, $int_name:tt, $float_name:tt) => {
        
        fn $name (lhs: &Variant, rhs: &Variant) -> Option<Variant> {
            let value = match (lhs, rhs) {
                (Variant::Integer(lhs_value), Variant::Integer(rhs_value)) => $int_name (*lhs_value, *rhs_value),
                _ if is_arithmetic_primitive(lhs) && is_arithmetic_primitive(rhs) => $float_name (lhs.float_value(), rhs.float_value()),
                _ => return None,
            };
            Some(value)
        }
        
    };
}

// Arithmeticis_bitwise_primitive

eval_binary_arithmetic!(eval_mul, int_mul, float_mul);
fn int_mul(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs * rhs) }
fn float_mul(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs * rhs) }

eval_binary_arithmetic!(eval_div, int_div, float_div);
fn int_div(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs / rhs) }
fn float_div(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs / rhs) }

eval_binary_arithmetic!(eval_mod, int_mod, float_mod);
fn int_mod(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs % rhs) }
fn float_mod(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs % rhs) }

eval_binary_arithmetic!(eval_add, int_add, float_add);
fn int_add(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs + rhs) }
fn float_add(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs + rhs) }

eval_binary_arithmetic!(eval_sub, int_sub, float_sub);
fn int_sub(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs - rhs) }
fn float_sub(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs - rhs) }

// Comparison - uses the same numeric coercion rules as Arithmetic

eval_binary_arithmetic!(eval_lt, int_lt, float_lt);
fn int_lt(lhs: IntType, rhs: IntType) -> Variant { Variant::Boolean(lhs < rhs) }
fn float_lt(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Boolean(lhs < rhs) }

fn eval_ge(lhs: &Variant, rhs: &Variant) -> Option<Variant> {
    Some(Variant::Boolean(!eval_lt(lhs, rhs)?.truth_value()))
}

eval_binary_arithmetic!(eval_le, int_le, float_le);
fn int_le(lhs: IntType, rhs: IntType) -> Variant { Variant::Boolean(lhs <= rhs) }
fn float_le(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Boolean(lhs <= rhs) }

fn eval_gt(lhs: &Variant, rhs: &Variant) -> Option<Variant> {
    Some(Variant::Boolean(!eval_le(lhs, rhs)?.truth_value()))
}

eval_binary_arithmetic!(eval_eq, int_eq, float_eq);
fn int_eq(lhs: IntType, rhs: IntType) -> Variant { Variant::Boolean(lhs == rhs) }
fn float_eq(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Boolean(lhs == rhs) }

fn eval_ne(lhs: &Variant, rhs: &Variant) -> Option<Variant> {
    Some(Variant::Boolean(!eval_eq(lhs, rhs)?.truth_value()))
}


// Bitwise

macro_rules! eval_binary_bitwise {
    ($name:tt, $bool_name:tt, $int_name:tt) => {
        
        fn $name (lhs: &Variant, rhs: &Variant) -> Option<Variant> {
            let value = match (lhs, rhs) {
                (Variant::Boolean(lhs_value), Variant::Boolean(rhs_value)) => $bool_name (*lhs_value, *rhs_value),
                _ if is_bitwise_primitive(lhs) && is_bitwise_primitive(rhs) => $int_name (lhs.bit_value(), rhs.bit_value()),
                _ => return None,
            };
            Some(value)
        }
        
    };
}

// for primitive bitshifts, if the LHS is boolean it is treated as 0/1 (instead of all 0s/all 1s)
macro_rules! eval_binary_shift {
    ($name:tt, $int_name:tt) => {
        
        fn $name (lhs: &Variant, rhs: &Variant) -> Option<Variant> {
            match (lhs, rhs) {
                (_, Variant::Integer(shift)) if is_bitwise_primitive(lhs) => Some($int_name (lhs.bit_value(), *shift)),
                (_, Variant::Boolean(true))  if is_bitwise_primitive(lhs) => Some($int_name (lhs.bit_value(), 1)),
                (_, Variant::Boolean(false)) if is_bitwise_primitive(lhs) => Some($int_name (lhs.bit_value(), 0)),
                _ => None,
            }
        }
        
    };
}

eval_binary_shift!(eval_shl, int_shl);
fn int_shl(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs << rhs) }

eval_binary_shift!(eval_shr, int_shr);
fn int_shr(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs >> rhs) }

eval_binary_bitwise!(eval_and, bool_and, int_and);
fn bool_and(lhs: bool, rhs: bool) -> Variant { Variant::Boolean(lhs & rhs) }
fn int_and(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs & rhs) }

eval_binary_bitwise!(eval_xor, bool_xor, int_xor);
fn bool_xor(lhs: bool, rhs: bool) -> Variant { Variant::Boolean(lhs ^ rhs) }
fn int_xor(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs ^ rhs) }

eval_binary_bitwise!(eval_or, bool_or, int_or);
fn bool_or(lhs: bool, rhs: bool) -> Variant { Variant::Boolean(lhs | rhs) }
fn int_or(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs | rhs) }
