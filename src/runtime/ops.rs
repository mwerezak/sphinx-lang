//! Binary and unary operations with certain primitive types will *short-circuit*,
//! meaning that the resulting value will be computed using the logic defined here
//! instead of deferring to the type system.
//!
//! For most functions in this module, returning None means that the operation
//! should be deferred to the metatables of the operands involved.

use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::errors::{ExecResult, ErrorKind};


#[inline(always)]
pub fn is_arithmetic_primitive(value: &Variant) -> bool {
    matches!(value, Variant::Integer(..) | Variant::Float(..))
}

#[inline(always)]
pub fn is_bitwise_primitive(value: &Variant) -> bool {
    matches!(value, Variant::BoolTrue | Variant::BoolFalse | Variant::Integer(..))
}


// Unary Operators

#[inline]
pub fn eval_neg(operand: &Variant) -> ExecResult<Option<Variant>> {
    let value = match operand {
        Variant::Integer(value) => (-value).into(),
        Variant::Float(value) => (-value).into(),
        _ => return Ok(None),
    };
    Ok(Some(value))
}

#[inline]
pub fn eval_pos(operand: &Variant) -> ExecResult<Option<Variant>> {
    let value = match operand {
        // No-op for arithmetic primitives
        Variant::Integer(value) => (*value).into(),
        Variant::Float(value) => (*value).into(),
        _ => return Ok(None),
    };
    Ok(Some(value))
}

#[inline]
pub fn eval_inv(operand: &Variant) -> ExecResult<Option<Variant>> {
    let value = match operand {
        Variant::BoolTrue => Variant::BoolFalse,
        Variant::BoolFalse => Variant::BoolTrue,
        Variant::Integer(value) => Variant::from(!value),
        _ => return Ok(None),
    };
    Ok(Some(value))
}

#[inline]
pub fn eval_not(operand: &Variant) -> ExecResult<Variant> {
    return Ok(Variant::from(!operand.truth_value()))
}


// Binary Operators

/// Equality is the most universally applicable of all the operands.
///
/// It's a bit different than all the other operations in that we can guarantee
/// a result (no "invalid operand errors"). This is because it is defined for all primitive types,
/// and for user objects that don't define __eq we can always fall back to reference equality.
/// Note: This property is really helpful for implementing tuple equality here.
#[inline]
pub fn eval_eq(lhs: &Variant, rhs: &Variant) -> bool {
    match (lhs, rhs) {
        // nil always compares false
        (Variant::Nil, _) => false,
        (_, Variant::Nil) => false,
        
        // empty tuple is only equal with itself
        (Variant::EmptyTuple, Variant::EmptyTuple) => true,
        
        (Variant::BoolTrue, Variant::BoolTrue) => true,
        (Variant::BoolFalse, Variant::BoolFalse) => true,
        
        // string equality
        (Variant::String(a), Variant::String(b)) => a == b,
        
        // numeric equality
        (Variant::Integer(a), Variant::Integer(b)) => *a == *b,
        
        (a, b) if is_arithmetic_primitive(a) && is_arithmetic_primitive(b) 
            => a.float_value().unwrap() == b.float_value().unwrap(),

        // tuple equality
        (Variant::Tuple(a), Variant::Tuple(b)) => {
            a.iter().zip(b.iter())
                .all(|(a, b)| eval_eq(a, b))
        },

        // TODO objects, defer to metatable or fall back to reference equality using GC handles

        _ => false,
    }
}

#[inline(always)]
pub fn eval_ne(lhs: &Variant, rhs: &Variant) -> bool {
    !eval_eq(lhs, rhs)
}



// Numeric Operations

// These are used to short-circuit the general metamethod lookup path to evaluate of binary operators
// They always succeed (due to the way the numeric coercion rules are set up), and hence don't use ExecResult. 
// Instead, they produce an Option and return None if the operands aren't the right type to short-circuit


// Arithmetic

macro_rules! eval_binary_arithmetic {
    ($name:tt, $int_name:tt, $float_name:tt) => {
        
        #[inline]
        pub fn $name (lhs: &Variant, rhs: &Variant) -> ExecResult<Option<Variant>> {
            let value = match (lhs, rhs) {
                (Variant::Integer(lhs_value), Variant::Integer(rhs_value)) => $int_name (*lhs_value, *rhs_value)?,
                _ if is_arithmetic_primitive(lhs) && is_arithmetic_primitive(rhs) 
                    => $float_name (lhs.float_value().unwrap(), rhs.float_value().unwrap())?,
                
                _ => return Ok(None),
            };
            Ok(Some(value))
        }
        
    };
}

// overflow check
macro_rules! checked_int_math {
    ( $method:tt, $lhs:expr, $rhs:expr ) => {
        match $lhs.$method($rhs) {
            (value, false) => Ok(Variant::Integer(value)),
            (_, true) => Err(ErrorKind::OverflowError.into()),
        }
    };
}

eval_binary_arithmetic!(eval_mul, int_mul, float_mul);
#[inline(always)] fn int_mul(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { checked_int_math!(overflowing_mul, lhs, rhs) }
#[inline(always)] fn float_mul(lhs: FloatType, rhs: FloatType) -> ExecResult<Variant> { Ok(Variant::Float(lhs * rhs)) }

eval_binary_arithmetic!(eval_div, int_div, float_div);
#[inline(always)] fn int_div(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { checked_int_math!(overflowing_div, lhs, rhs) }
#[inline(always)] fn float_div(lhs: FloatType, rhs: FloatType) -> ExecResult<Variant> { Ok(Variant::Float(lhs / rhs)) }

eval_binary_arithmetic!(eval_mod, int_mod, float_mod);
#[inline(always)] fn int_mod(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { Ok(Variant::Integer(lhs % rhs)) }
#[inline(always)] fn float_mod(lhs: FloatType, rhs: FloatType) -> ExecResult<Variant> { Ok(Variant::Float(lhs % rhs)) }

eval_binary_arithmetic!(eval_add, int_add, float_add);
#[inline(always)] fn int_add(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { checked_int_math!(overflowing_add, lhs, rhs) }
#[inline(always)] fn float_add(lhs: FloatType, rhs: FloatType) -> ExecResult<Variant> { Ok(Variant::Float(lhs + rhs)) }

eval_binary_arithmetic!(eval_sub, int_sub, float_sub);
#[inline(always)] fn int_sub(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { checked_int_math!(overflowing_sub, lhs, rhs) }
#[inline(always)] fn float_sub(lhs: FloatType, rhs: FloatType) -> ExecResult<Variant> { Ok(Variant::Float(lhs - rhs)) }

// Comparison - uses similar coercion rules as Arithmetic, may only produce boolean results
macro_rules! eval_binary_comparison {
    ($name:tt, $int_name:tt, $float_name:tt) => {
        
        #[inline]
        pub fn $name (lhs: &Variant, rhs: &Variant) -> Option<bool> {
            let value = match (lhs, rhs) {
                (Variant::Integer(lhs_value), Variant::Integer(rhs_value)) => $int_name (*lhs_value, *rhs_value),
                _ if is_arithmetic_primitive(lhs) && is_arithmetic_primitive(rhs) 
                    => $float_name (lhs.float_value().unwrap(), rhs.float_value().unwrap()),
                
                _ => return None,
            };
            Some(value)
        }
        
    };
}

eval_binary_comparison!(eval_lt, int_lt, float_lt);
#[inline(always)] fn int_lt(lhs: IntType, rhs: IntType) -> bool { lhs < rhs }
#[inline(always)] fn float_lt(lhs: FloatType, rhs: FloatType) -> bool { lhs < rhs }

#[inline(always)]
pub fn eval_ge(lhs: &Variant, rhs: &Variant) -> Option<bool> { Some(!eval_lt(lhs, rhs)?) }


eval_binary_comparison!(eval_le, int_le, float_le);
#[inline(always)] fn int_le(lhs: IntType, rhs: IntType) -> bool { lhs <= rhs }
#[inline(always)] fn float_le(lhs: FloatType, rhs: FloatType) -> bool { lhs <= rhs }

#[inline(always)]
pub fn eval_gt(lhs: &Variant, rhs: &Variant) -> Option<bool> { Some(!eval_le(lhs, rhs)?) }

// equality is handled specially, so this only applies to primitive numerics


// Bitwise Operations

macro_rules! eval_binary_bitwise {
    ($name:tt, $bool_name:tt, $int_name:tt) => {
        
        #[inline]
        pub fn $name (lhs: &Variant, rhs: &Variant) -> Option<Variant> {
            let value = match (lhs, rhs) {
                (Variant::BoolTrue, Variant::BoolTrue) => $bool_name (true, true),
                (Variant::BoolTrue, Variant::BoolFalse) => $bool_name (true, false),
                (Variant::BoolFalse, Variant::BoolTrue) => $bool_name (false, true),
                (Variant::BoolFalse, Variant::BoolFalse) => $bool_name (false, false),
                _ if is_bitwise_primitive(lhs) && is_bitwise_primitive(rhs) 
                    => $int_name (lhs.bit_value().unwrap(), rhs.bit_value().unwrap()),
                
                _ => return None,
            };
            Some(value)
        }
        
    };
}

eval_binary_bitwise!(eval_and, bool_and, int_and);
#[inline(always)] fn bool_and(lhs: bool, rhs: bool) -> Variant { (lhs & rhs).into() }
#[inline(always)] fn int_and(lhs: IntType, rhs: IntType) -> Variant { (lhs & rhs).into() }

eval_binary_bitwise!(eval_xor, bool_xor, int_xor);
#[inline(always)] fn bool_xor(lhs: bool, rhs: bool) -> Variant { (lhs ^ rhs).into() }
#[inline(always)] fn int_xor(lhs: IntType, rhs: IntType) -> Variant { (lhs ^ rhs).into() }

eval_binary_bitwise!(eval_or, bool_or, int_or);
#[inline(always)] fn bool_or(lhs: bool, rhs: bool) -> Variant { (lhs | rhs).into() }
#[inline(always)] fn int_or(lhs: IntType, rhs: IntType) -> Variant { (lhs | rhs).into() }


// Bit Shifts

// for primitive bitshifts, if the LHS is boolean it is treated as 0/1 i.e. do a shift, or not (instead of all 0s/all 1s for the bitwise ops)
macro_rules! eval_binary_shift {
    ($name:tt, $int_name:tt) => {
        
        #[inline]
        pub fn $name (lhs: &Variant, rhs: &Variant) -> ExecResult<Option<Variant>> {
            let value = match (lhs, rhs) {
                (_, Variant::Integer(shift)) if is_bitwise_primitive(lhs) => $int_name (lhs.bit_value().unwrap(), *shift)?,
                (_, Variant::BoolTrue)  if is_bitwise_primitive(lhs) => $int_name (lhs.bit_value().unwrap(), 1)?,
                (_, Variant::BoolFalse) if is_bitwise_primitive(lhs) => lhs.clone(), // no-op, just copy the value to output
                _ => return Ok(None),
            };
            Ok(Some(value))
        }
        
    };
}

eval_binary_shift!(eval_shl, int_shl);
#[inline]
fn int_shl(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { 
    if rhs < 0 { 
        return Err(ErrorKind::NegativeShiftCount.into()); 
    }
    checked_int_math!(overflowing_shl, lhs, rhs.try_into().unwrap()) 
}

eval_binary_shift!(eval_shr, int_shr);
#[inline]
fn int_shr(lhs: IntType, rhs: IntType) -> ExecResult<Variant> {
    if rhs < 0 { 
        return Err(ErrorKind::NegativeShiftCount.into()); 
    }
    checked_int_math!(overflowing_shr, lhs, rhs.try_into().unwrap()) 
}