// Binary and unary operations with certain primitive types will *short-circuit*,
// meaning that the resulting value will be computed using the logic defined in this module
// instead of deferring to the type system

use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison};
use crate::interpreter::errors::{EvalResult, EvalError, EvalErrorKind};


pub fn is_arithmetic_primitive(value: &Variant) -> bool {
    matches!(value, Variant::Integer(..) | Variant::Float(..))
}

pub fn is_bitwise_primitive(value: &Variant) -> bool {
    matches!(value, Variant::Boolean(..) | Variant::Integer(..))
}

// using macros because lots of boilerplate

macro_rules! checked_int_math {  // overflow check
    ( $method:tt, $lhs:expr, $rhs:expr ) => {
        match $lhs.$method($rhs) {
            (value, false) => Ok(Variant::Integer(value)),
            (_, true) => Err(EvalErrorKind::OverflowError.into()),
        }
    };
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
        BinaryOp::Arithmetic(op) => match op {
            Arithmetic::Mul    => eval_mul(&lhs, &rhs)?,
            Arithmetic::Div    => eval_div(&lhs, &rhs)?,
            Arithmetic::Mod    => eval_mod(&lhs, &rhs)?,
            Arithmetic::Add    => eval_add(&lhs, &rhs)?,
            Arithmetic::Sub    => eval_sub(&lhs, &rhs)?,
        },
        
        BinaryOp::Bitwise(op) => match op {
            Bitwise::And => eval_and(&lhs, &rhs),
            Bitwise::Xor => eval_xor(&lhs, &rhs),
            Bitwise::Or  => eval_or(&lhs, &rhs),
        },
        
        BinaryOp::Shift(op) => match op {
            Shift::Left => eval_shl(&lhs, &rhs)?,
            Shift::Right => eval_shr(&lhs, &rhs)?,
        }
        
        BinaryOp::Comparison(op) => eval_comparison(op, lhs, rhs).map(Variant::Boolean),
        
        _ => None,
    };
    
    if let Some(value) = result {
        Ok(value)
    } else {
        eval_binary_other(op, lhs, rhs)
    }
}

fn eval_comparison(op: Comparison, lhs: &Variant, rhs: &Variant) -> Option<bool> {
    match op {
        Comparison::LT     => eval_lt(&lhs, &rhs),
        Comparison::GT     => eval_gt(&lhs, &rhs),
        Comparison::LE     => eval_le(&lhs, &rhs),
        Comparison::GE     => eval_ge(&lhs, &rhs),
        
        Comparison::EQ     => Some(eval_equals(&lhs, &rhs)),
        Comparison::NE     => Some(!eval_equals(&lhs, &rhs)),
    }
}

// Equality is handled specially
// Note that even for GCObject we will always get a bool because the default is to fallback to reference equality
// so there is no case where comparing two values for equality will fail, unlike most operators

fn eval_equals(lhs: &Variant, rhs: &Variant) -> bool {
    match (lhs, rhs) {
        // nil always compares false
        (Variant::Nil, _) => false,
        (_, Variant::Nil) => false,
        
        // empty tuple is only equal with itself
        (Variant::EmptyTuple, Variant::EmptyTuple) => true,
        
        // shortcut for interned strings, boolean
        (Variant::Boolean(lhs_value), Variant::Boolean(rhs_value)) => *lhs_value == *rhs_value,
        (Variant::InternStr(lhs_value), Variant::InternStr(rhs_value)) => *lhs_value == *rhs_value,
        
        // numeric equality
        (Variant::Integer(lhs_value), Variant::Integer(rhs_value)) => *lhs_value == *rhs_value,
        (_, _) if is_arithmetic_primitive(lhs) && is_arithmetic_primitive(rhs) => lhs.float_value() == rhs.float_value(),
        
        // TODO GCObject

        _ => false,
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


// Arithmetic

macro_rules! eval_binary_arithmetic {
    ($name:tt, $int_name:tt, $float_name:tt) => {
        
        fn $name (lhs: &Variant, rhs: &Variant) -> EvalResult<Option<Variant>> {
            let value = match (lhs, rhs) {
                (Variant::Integer(lhs_value), Variant::Integer(rhs_value)) => $int_name (*lhs_value, *rhs_value)?,
                _ if is_arithmetic_primitive(lhs) && is_arithmetic_primitive(rhs) => $float_name (lhs.float_value(), rhs.float_value())?,
                _ => return Ok(None),
            };
            Ok(Some(value))
        }
        
    };
}

eval_binary_arithmetic!(eval_mul, int_mul, float_mul);
fn int_mul(lhs: IntType, rhs: IntType) -> EvalResult<Variant> { checked_int_math!(overflowing_mul, lhs, rhs) }
fn float_mul(lhs: FloatType, rhs: FloatType) -> EvalResult<Variant> { Ok(Variant::Float(lhs * rhs)) }

eval_binary_arithmetic!(eval_div, int_div, float_div);
fn int_div(lhs: IntType, rhs: IntType) -> EvalResult<Variant> { checked_int_math!(overflowing_div, lhs, rhs) }
fn float_div(lhs: FloatType, rhs: FloatType) -> EvalResult<Variant> { Ok(Variant::Float(lhs / rhs)) }

eval_binary_arithmetic!(eval_mod, int_mod, float_mod);
fn int_mod(lhs: IntType, rhs: IntType) -> EvalResult<Variant> { Ok(Variant::Integer(lhs % rhs)) }
fn float_mod(lhs: FloatType, rhs: FloatType) -> EvalResult<Variant> { Ok(Variant::Float(lhs % rhs)) }

eval_binary_arithmetic!(eval_add, int_add, float_add);
fn int_add(lhs: IntType, rhs: IntType) -> EvalResult<Variant> { checked_int_math!(overflowing_add, lhs, rhs) }
fn float_add(lhs: FloatType, rhs: FloatType) -> EvalResult<Variant> { Ok(Variant::Float(lhs + rhs)) }

eval_binary_arithmetic!(eval_sub, int_sub, float_sub);
fn int_sub(lhs: IntType, rhs: IntType) -> EvalResult<Variant> { checked_int_math!(overflowing_sub, lhs, rhs) }
fn float_sub(lhs: FloatType, rhs: FloatType) -> EvalResult<Variant> { Ok(Variant::Float(lhs - rhs)) }

// Comparison - uses similar coercion rules as Arithmetic, may only produce boolean results
macro_rules! eval_binary_comparison {
    ($name:tt, $int_name:tt, $float_name:tt) => {
        
        fn $name (lhs: &Variant, rhs: &Variant) -> Option<bool> {
            let value = match (lhs, rhs) {
                (Variant::Integer(lhs_value), Variant::Integer(rhs_value)) => $int_name (*lhs_value, *rhs_value),
                _ if is_arithmetic_primitive(lhs) && is_arithmetic_primitive(rhs) => $float_name (lhs.float_value(), rhs.float_value()),
                _ => return None,
            };
            Some(value)
        }
        
    };
}

eval_binary_comparison!(eval_lt, int_lt, float_lt);
fn int_lt(lhs: IntType, rhs: IntType) -> bool { lhs < rhs }
fn float_lt(lhs: FloatType, rhs: FloatType) -> bool { lhs < rhs }

fn eval_ge(lhs: &Variant, rhs: &Variant) -> Option<bool> { Some(!eval_lt(lhs, rhs)?) }


eval_binary_comparison!(eval_le, int_le, float_le);
fn int_le(lhs: IntType, rhs: IntType) -> bool { lhs <= rhs }
fn float_le(lhs: FloatType, rhs: FloatType) -> bool { lhs <= rhs }

fn eval_gt(lhs: &Variant, rhs: &Variant) -> Option<bool> { Some(!eval_le(lhs, rhs)?) }

// equality is handled specially, so this only applies to primitive numerics


// Bitwise Operations

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

eval_binary_bitwise!(eval_and, bool_and, int_and);
fn bool_and(lhs: bool, rhs: bool) -> Variant { Variant::Boolean(lhs & rhs) }
fn int_and(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs & rhs) }

eval_binary_bitwise!(eval_xor, bool_xor, int_xor);
fn bool_xor(lhs: bool, rhs: bool) -> Variant { Variant::Boolean(lhs ^ rhs) }
fn int_xor(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs ^ rhs) }

eval_binary_bitwise!(eval_or, bool_or, int_or);
fn bool_or(lhs: bool, rhs: bool) -> Variant { Variant::Boolean(lhs | rhs) }
fn int_or(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs | rhs) }


// Bit Shifts

// for primitive bitshifts, if the LHS is boolean it is treated as 0/1 i.e. do a shift, or not (instead of all 0s/all 1s for the bitwise ops)
macro_rules! eval_binary_shift {
    ($name:tt, $int_name:tt) => {
        
        fn $name (lhs: &Variant, rhs: &Variant) -> EvalResult<Option<Variant>> {
            let value = match (lhs, rhs) {
                (_, Variant::Integer(shift)) if is_bitwise_primitive(lhs) => $int_name (lhs.bit_value(), *shift)?,
                (_, Variant::Boolean(true))  if is_bitwise_primitive(lhs) => $int_name (lhs.bit_value(), 1)?,
                (_, Variant::Boolean(false)) if is_bitwise_primitive(lhs) => *lhs, // no-op
                _ => return Ok(None),
            };
            Ok(Some(value))
        }
        
    };
}

eval_binary_shift!(eval_shl, int_shl);
fn int_shl(lhs: IntType, rhs: IntType) -> EvalResult<Variant> { 
    if rhs < 0 { 
        return Err(EvalErrorKind::NegativeShiftCount.into()); 
    }
    checked_int_math!(overflowing_shl, lhs, rhs.try_into().unwrap()) 
}

eval_binary_shift!(eval_shr, int_shr);
fn int_shr(lhs: IntType, rhs: IntType) -> EvalResult<Variant> {
    if rhs < 0 { 
        return Err(EvalErrorKind::NegativeShiftCount.into()); 
    }
    checked_int_math!(overflowing_shr, lhs, rhs.try_into().unwrap()) 
}