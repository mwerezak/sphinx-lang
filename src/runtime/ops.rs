//! Binary and unary operations with certain primitive types will *short-circuit*,
//! meaning that the resulting value will be computed using the logic defined here
//! instead of deferring to the type system.
//!
//! For most functions in this module, returning None means that the operation
//! should be deferred to the metatables of the operands involved.

use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::gc::GC;
use crate::runtime::types::MetaObject;
use crate::runtime::types::metatable::{UnaryTag, BinaryTag, CompareTag};
use crate::runtime::errors::{ExecResult, ErrorKind};


#[inline(always)]
pub fn is_arithmetic_primitive(value: &Variant) -> bool {
    matches!(value, Variant::Integer(..) | Variant::Float(..))
}

#[inline(always)]
pub fn is_bitwise_primitive(value: &Variant) -> bool {
    matches!(value, Variant::BoolTrue | Variant::BoolFalse | Variant::Integer(..))
}

// Metamethod Fallbacks

macro_rules! meta_eval_unary {
    ( $operand:expr, $unary_method:tt ) => {
        $operand.as_meta().$unary_method()
            .unwrap_or_else(|| Err(ErrorKind::InvalidUnaryOperand($operand.type_tag()).into()))
    };
}

macro_rules! meta_eval_binary {
    ( $lhs:expr, $rhs:expr, $binary_method:tt, $reflected_method:tt) => {
        {
            if let Some(result) = $lhs.as_meta().$binary_method($rhs) {
                return result;
            }
            
            if $lhs.type_tag() != $rhs.type_tag() {
                if let Some(result) = $rhs.as_meta().$reflected_method($lhs) {
                    return result;
                }
            }
            
            Err(ErrorKind::InvalidBinaryOperand($lhs.type_tag(), $rhs.type_tag()).into())
        }
    };
}

macro_rules! meta_eval_inequality {
    ( $lhs:expr, $rhs:expr, $compare_method:tt, $reflected_method:tt) => {
        {
            if let Some(result) = $lhs.as_meta().$compare_method($rhs) {
                return result;
            }
            
            if $lhs.type_tag() != $rhs.type_tag() {
                if let Some(result) = $rhs.as_meta().$reflected_method($lhs) {
                    return result.map(|cmp| !cmp);
                }
            }
            
            Err(ErrorKind::InvalidBinaryOperand($lhs.type_tag(), $rhs.type_tag()).into())
        }
    };
}

impl Variant {
    
    // Unary
    
    #[inline(always)]
    pub fn apply_neg(&self) -> ExecResult<Variant> {
        match self {
            Self::Integer(value) => value.apply_neg().unwrap(),
            Self::Float(value) => value.apply_neg().unwrap(),
            _ => meta_eval_unary!(self, apply_neg),
        }
    }
    
    #[inline(always)]
    pub fn apply_pos(&self) -> ExecResult<Variant> {
        match self {
            Self::Integer(value) => value.apply_pos().unwrap(),
            Self::Float(value) => value.apply_pos().unwrap(),
            _ => meta_eval_unary!(self, apply_pos),
        }
    }
    
    #[inline(always)]
    pub fn apply_inv(&self) -> ExecResult<Variant> {
        match self {
            Self::BoolTrue => true.apply_inv().unwrap(),
            Self::BoolFalse => false.apply_inv().unwrap(),
            Self::Integer(value) => value.apply_inv().unwrap(),
            _ => meta_eval_unary!(self, apply_inv)
        }
    }
    
    #[inline(always)]
    pub fn apply_not(&self) -> ExecResult<Variant> {
        match self {
            Self::BoolTrue => Ok(Self::BoolFalse),
            Self::BoolFalse => Ok(Self::BoolTrue),
            _ => Ok(Variant::from(!self.as_bool()?)),
        }
    }
    
    // Arithmetic
    
    #[inline(always)]
    pub fn apply_mul(&self, rhs: &Variant) -> ExecResult<Variant> {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(..)) => lhs.apply_mul(rhs).unwrap(),
            _ => meta_eval_binary!(self, rhs, apply_mul, apply_rmul),
        }
    }
    
    #[inline(always)]
    pub fn apply_div(&self, rhs: &Variant) -> ExecResult<Variant> {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(..)) => lhs.apply_div(rhs).unwrap(),
            _ => meta_eval_binary!(self, rhs, apply_div, apply_rdiv),
        }
    }
    
    #[inline(always)]
    pub fn apply_mod(&self, rhs: &Variant) -> ExecResult<Variant> {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(..)) => lhs.apply_mod(rhs).unwrap(),
            _ => meta_eval_binary!(self, rhs, apply_mod, apply_rmod),
        }
    }
    
    #[inline(always)]
    pub fn apply_add(&self, rhs: &Variant) -> ExecResult<Variant> {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(..)) => lhs.apply_add(rhs).unwrap(),
            _ => meta_eval_binary!(self, rhs, apply_add, apply_radd),
        }
    }
    
    #[inline(always)]
    pub fn apply_sub(&self, rhs: &Variant) -> ExecResult<Variant> {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(..)) => lhs.apply_sub(rhs).unwrap(),
            _ => meta_eval_binary!(self, rhs, apply_sub, apply_rsub),
        }
    }
    
    // Bitwise
    
    pub fn apply_and(&self, rhs: &Variant) -> ExecResult<Variant> {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(..)) => lhs.apply_and(rhs).unwrap(),
            _ => meta_eval_binary!(self, rhs, apply_and, apply_rand),
        }
    }
    
    pub fn apply_xor(&self, rhs: &Variant) -> ExecResult<Variant> {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(..)) => lhs.apply_xor(rhs).unwrap(),
            _ => meta_eval_binary!(self, rhs, apply_xor, apply_rxor),
        }
    }
    
    pub fn apply_or(&self, rhs: &Variant) -> ExecResult<Variant> {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(..)) => lhs.apply_or(rhs).unwrap(),
            _ => meta_eval_binary!(self, rhs, apply_or, apply_ror),
        }
    }
    
    // Shifts
    
    pub fn apply_shl(&self, rhs: &Variant) -> ExecResult<Variant> {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(..)) => lhs.apply_shl(rhs).unwrap(),
            _ => meta_eval_binary!(self, rhs, apply_shl, apply_rshl),
        }
    }
    
    pub fn apply_shr(&self, rhs: &Variant) -> ExecResult<Variant> {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(..)) => lhs.apply_shr(rhs).unwrap(),
            _ => meta_eval_binary!(self, rhs, apply_shr, apply_rshr),
        }
    }

    // Comparison
    
    pub fn cmp_eq(&self, other: &Variant) -> ExecResult<bool> {
        match (self, other) {
            (Variant::Nil, Variant::Nil) => Ok(true),
            (Variant::EmptyTuple, Variant::EmptyTuple) => Ok(true),
            
            (Variant::BoolTrue,  Variant::BoolTrue)  => Ok(true),
            (Variant::BoolFalse, Variant::BoolFalse) => Ok(true),
            (Variant::BoolTrue,  Variant::BoolFalse) => Ok(false),
            (Variant::BoolFalse, Variant::BoolTrue)  => Ok(false),
            
            (Variant::String(a), Variant::String(b)) => Ok(*a == *b),
            (Variant::Integer(a), Variant::Integer(b)) => Ok(*a == *b),
            _ => {
                if let Some(result) = self.as_meta().cmp_eq(other) {
                    return result;
                }
                
                if self.type_tag() != other.type_tag() {
                    if let Some(result) = other.as_meta().cmp_eq(self) {
                        return result;
                    }
                }
                
                Ok(false)
            }
        }
        
    }
    
    pub fn cmp_ne(&self, other: &Variant) -> ExecResult<bool> {
        self.cmp_eq(other).map(|cmp| !cmp)
    }
    
    pub fn cmp_lt(&self, other: &Variant) -> ExecResult<bool> {
        match (self, other) {
            (Self::Integer(this), Self::Integer(..)) => this.cmp_lt(other).unwrap(),
            (Self::Float(this), Self::Float(..)) => this.cmp_lt(other).unwrap(),
            _ => meta_eval_inequality!(self, other, cmp_lt, cmp_le)
        }
    }
    
    pub fn cmp_le(&self, other: &Variant) -> ExecResult<bool> {
        match (self, other) {
            (Self::Integer(this), Self::Integer(..)) => this.cmp_le(other).unwrap(),
            (Self::Float(this), Self::Float(..)) => this.cmp_le(other).unwrap(),
            _ => meta_eval_inequality!(self, other, cmp_le, cmp_lt)
        }
    }
    
    pub fn cmp_gt(&self, other: &Variant) -> ExecResult<bool> {
        self.cmp_le(other).map(|cmp| !cmp)
    }

    pub fn cmp_ge(&self, other: &Variant) -> ExecResult<bool> {
        self.cmp_lt(other).map(|cmp| !cmp)
    }
}


/*
/*
#[inline]
fn eval_meta_unary(_tag: UnaryTag, operand: &Variant) -> ExecResult<Variant> {
    Err(ErrorKind::InvalidUnaryOperand(operand.type_tag()).into())
    // let op_func = operand.metatable().op_unary(tag)
    //     .ok_or_else(|| ErrorKind::InvalidUnaryOperand(*operand))?;
    
    // op_func(operand)
}

fn eval_meta_binary(_tag: BinaryTag, lhs: &Variant, rhs: &Variant) -> ExecResult<Variant> {
    Err(ErrorKind::InvalidBinaryOperand(lhs.type_tag(), rhs.type_tag()).into())
    // let result = lhs.metatable().op_binary(tag)
    //     .map(|op_func| op_func(lhs, rhs));
    
    // match result {
    //     Some(Err(error)) => return Err(error),
    //     Some(Ok(Some(value))) => return Ok(value),
    //     _ => { }
    // }
    
    // let reflected_func = rhs.metatable().op_binary_reflected(tag)
    //     .ok_or_else(|| ErrorKind::InvalidBinaryOperand(*lhs, *rhs))?;
    
    // reflected_func(rhs, lhs)
}

fn eval_meta_comparison(_tag: CompareTag, lhs: &Variant, rhs: &Variant) -> ExecResult<bool> {
    Err(ErrorKind::InvalidBinaryOperand(lhs.type_tag(), rhs.type_tag()).into())
    // let result = lhs.metatable().op_compare(tag)
    //     .map(|op_func| op_func(lhs, rhs));
    
    // match result {
    //     Some(Err(error)) => return Err(error),
    //     Some(Ok(Some(value))) => return Ok(value),
    //     _ => { }
    // }
    
    // let reflected_tag = match tag {
    //     CompareTag::LT => CompareTag::LE,
    //     CompareTag::LE => CompareTag::LT,
    //     CompareTag::EQ => CompareTag::EQ,
    // };
    
    // let result = rhs.metatable().op_compare(reflected_tag)
    //     .map(|op_func| op_func(rhs, lhs));
    
    // match result {
    //     Some(Err(error)) => Err(error),
    //     Some(Ok(Some(value))) => Ok(value),
        
    //     // equality always succeeds
    //     _ if tag == CompareTag::EQ => {
    //         // fall back to reference equality for GC types
    //         let result = lhs.as_gc()
    //             .and_then(|lhs| rhs.as_gc().map(|rhs| (lhs, rhs)))
    //             .map_or(false, |(lhs, rhs)| GC::ptr_eq(&lhs, &rhs));
            
    //         Ok(result)
    //     }, 
        
    //     _ => Err(ErrorKind::InvalidBinaryOperand(*lhs, *rhs).into()),
    // }
}
*/

// Unary Operators
/*
#[inline]
pub fn eval_neg(operand: &Variant) -> ExecResult<Variant> {
    meta_eval_unary!(operand, apply_neg)
}

#[inline]
pub fn eval_pos(operand: &Variant) -> ExecResult<Variant> {
    meta_eval_unary!(operand, apply_pos)
}

#[inline]
pub fn eval_inv(operand: &Variant) -> ExecResult<Variant> {
    meta_eval_unary!(operand, apply_inv)
}

#[inline]
pub fn eval_not(operand: &Variant) -> ExecResult<Variant> {
    Ok(Variant::from(!operand.truth_value()))
}
*/

// Binary Operators

/// Equality is the most universally applicable of all the operands.
///
/// It's a bit different than all the other operations in that we can guarantee
/// a result (no "invalid operand errors"). This is because it is defined for all primitive types,
/// and for user objects that don't define __eq we can always fall back to reference equality.
/// Note: This property is really helpful for implementing tuple equality here.
#[inline]
pub fn eval_eq(lhs: &Variant, rhs: &Variant) -> ExecResult<bool> {
    let value = match (lhs, rhs) {
        
        (Variant::Nil, Variant::Nil) => true,
        (Variant::EmptyTuple, Variant::EmptyTuple) => true,
        
        (Variant::BoolTrue,  Variant::BoolTrue)  => true,
        (Variant::BoolFalse, Variant::BoolFalse) => true,
        (Variant::BoolTrue,  Variant::BoolFalse) => false,
        (Variant::BoolFalse, Variant::BoolTrue)  => false,
        
        // string equality
        (Variant::String(a), Variant::String(b)) => a == b,
        
        // numeric equality
        (Variant::Integer(a), Variant::Integer(b)) => *a == *b,
        (a, b) if is_arithmetic_primitive(a) && is_arithmetic_primitive(b) 
            => a.as_float().unwrap() == b.as_float().unwrap(),
        
        // tuple equality
        (Variant::Tuple(a), Variant::Tuple(b)) if a.len() == b.len() => {
            let a_items = a.iter();
            let b_items = b.iter();
            for (a, b) in a_items.zip(b_items) {
                if !eval_eq(a, b)? {
                    return Ok(false);
                }
            }
            true
        },
        (Variant::Tuple(..), Variant::Tuple(..)) => false,

        // TODO objects, defer to metatable or fall back to reference equality using GC handles
        (a, b) => return eval_meta_comparison(CompareTag::EQ, a, b),
    };
    Ok(value)
}

#[inline(always)]
pub fn eval_ne(lhs: &Variant, rhs: &Variant) -> ExecResult<bool> {
    Ok(!eval_eq(lhs, rhs)?)
}



// Numeric Operations

// Arithmetic

macro_rules! eval_binary_arithmetic {
    ($name:tt, $int_name:tt, $float_name:tt, $meta_method:tt, $reflected_method:tt ) => {
        
        #[inline]
        pub fn $name (lhs: &Variant, rhs: &Variant) -> ExecResult<Variant> {
            let lhs_value = lhs.as_int();
            let rhs_value = rhs.as_int();
            if lhs_value.and(rhs_value).is_some() {
                return $int_name (lhs_value.unwrap(), rhs_value.unwrap());
            }
            
            let lhs_value = lhs.as_float();
            let rhs_value = rhs.as_float();
            if lhs_value.and(rhs_value).is_some() {
                return $float_name (lhs_value.unwrap(), rhs_value.unwrap());
            }
            
            meta_eval_binary!(lhs, rhs, $meta_method, $reflected_method)
        }
        
    };
}

// overflow check
macro_rules! checked_int_math {
    ( $method:tt, $lhs:expr, $rhs:expr ) => {
        match $lhs.$method($rhs) {
            Some(value) => Ok(Variant::Integer(value)),
            None => Err(ErrorKind::OverflowError.into()),
        }
    };
}

eval_binary_arithmetic!(eval_mul, int_mul, float_mul, apply_mul, apply_rmul);
#[inline(always)] fn int_mul(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { checked_int_math!(checked_mul, lhs, rhs) }
#[inline(always)] fn float_mul(lhs: FloatType, rhs: FloatType) -> ExecResult<Variant> { Ok(Variant::Float(lhs * rhs)) }

eval_binary_arithmetic!(eval_div, int_div, float_div, apply_div, apply_rdiv);
#[inline(always)] fn int_div(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { 
    match lhs.checked_div(rhs) {
        Some(value) => Ok(Variant::Integer(value)),
        None if rhs == 0 => Err(ErrorKind::DivideByZero.into()),
        None => Err(ErrorKind::OverflowError.into()),
    }
}
#[inline(always)] fn float_div(lhs: FloatType, rhs: FloatType) -> ExecResult<Variant> { Ok(Variant::Float(lhs / rhs)) }

eval_binary_arithmetic!(eval_mod, int_mod, float_mod, apply_mod, apply_rmod);
#[inline(always)] fn int_mod(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { Ok(Variant::Integer(lhs % rhs)) }
#[inline(always)] fn float_mod(lhs: FloatType, rhs: FloatType) -> ExecResult<Variant> { Ok(Variant::Float(lhs % rhs)) }

eval_binary_arithmetic!(eval_add, int_add, float_add, apply_add, apply_radd);
#[inline(always)] fn int_add(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { checked_int_math!(checked_add, lhs, rhs) }
#[inline(always)] fn float_add(lhs: FloatType, rhs: FloatType) -> ExecResult<Variant> { Ok(Variant::Float(lhs + rhs)) }

eval_binary_arithmetic!(eval_sub, int_sub, float_sub, apply_sub, apply_rsub);
#[inline(always)] fn int_sub(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { checked_int_math!(checked_sub, lhs, rhs) }
#[inline(always)] fn float_sub(lhs: FloatType, rhs: FloatType) -> ExecResult<Variant> { Ok(Variant::Float(lhs - rhs)) }

// Comparison - uses similar coercion rules as Arithmetic, may only produce boolean results
macro_rules! eval_binary_comparison {
    ($name:tt, $int_name:tt, $float_name:tt, $tag:expr) => {
        
        #[inline]
        pub fn $name (lhs: &Variant, rhs: &Variant) -> ExecResult<bool> {
            
            let lhs_value = lhs.as_int();
            let rhs_value = rhs.as_int();
            if lhs_value.and(rhs_value).is_some() {
                return Ok($int_name (lhs_value.unwrap(), rhs_value.unwrap()));
            }
            
            let lhs_value = lhs.as_float();
            let rhs_value = rhs.as_float();
            if lhs_value.and(rhs_value).is_some() {
                return Ok($float_name (lhs_value.unwrap(), rhs_value.unwrap()));
            }
            
            eval_meta_comparison($tag, lhs, rhs)
        }
        
    };
}

eval_binary_comparison!(eval_lt, int_lt, float_lt, CompareTag::LT);
#[inline(always)] fn int_lt(lhs: IntType, rhs: IntType) -> bool { lhs < rhs }
#[inline(always)] fn float_lt(lhs: FloatType, rhs: FloatType) -> bool { lhs < rhs }

#[inline(always)]
pub fn eval_ge(lhs: &Variant, rhs: &Variant) -> ExecResult<bool> { Ok(!eval_lt(lhs, rhs)?) }


eval_binary_comparison!(eval_le, int_le, float_le, CompareTag::LE);
#[inline(always)] fn int_le(lhs: IntType, rhs: IntType) -> bool { lhs <= rhs }
#[inline(always)] fn float_le(lhs: FloatType, rhs: FloatType) -> bool { lhs <= rhs }

#[inline(always)]
pub fn eval_gt(lhs: &Variant, rhs: &Variant) -> ExecResult<bool> { Ok(!eval_le(lhs, rhs)?) }

// equality is handled specially, so this only applies to primitive numerics


// Bitwise Operations

macro_rules! eval_binary_bitwise {
    ($name:tt, $bool_name:tt, $int_name:tt, $tag:expr) => {
        
        #[inline]
        pub fn $name (lhs: &Variant, rhs: &Variant) -> ExecResult<Variant> {
            match (lhs, rhs) {
                (Variant::BoolTrue, Variant::BoolTrue) => Ok($bool_name (true, true)),
                (Variant::BoolTrue, Variant::BoolFalse) => Ok($bool_name (true, false)),
                (Variant::BoolFalse, Variant::BoolTrue) => Ok($bool_name (false, true)),
                (Variant::BoolFalse, Variant::BoolFalse) => Ok($bool_name (false, false)),
                
                _ => {
                    let lhs_value = lhs.as_bits();
                    let rhs_value = rhs.as_bits();
                    if lhs_value.and(rhs_value).is_some() {
                        Ok($int_name (lhs_value.unwrap(), rhs_value.unwrap()))
                    } else {
                        eval_meta_binary($tag, lhs, rhs)
                    }
                },
            }
        }
        
    };
}

eval_binary_bitwise!(eval_and, bool_and, int_and, BinaryTag::And);
#[inline(always)] fn bool_and(lhs: bool, rhs: bool) -> Variant { (lhs & rhs).into() }
#[inline(always)] fn int_and(lhs: IntType, rhs: IntType) -> Variant { (lhs & rhs).into() }

eval_binary_bitwise!(eval_xor, bool_xor, int_xor, BinaryTag::Xor);
#[inline(always)] fn bool_xor(lhs: bool, rhs: bool) -> Variant { (lhs ^ rhs).into() }
#[inline(always)] fn int_xor(lhs: IntType, rhs: IntType) -> Variant { (lhs ^ rhs).into() }

eval_binary_bitwise!(eval_or, bool_or, int_or, BinaryTag::Or);
#[inline(always)] fn bool_or(lhs: bool, rhs: bool) -> Variant { (lhs | rhs).into() }
#[inline(always)] fn int_or(lhs: IntType, rhs: IntType) -> Variant { (lhs | rhs).into() }


// Bit Shifts

// for primitive bitshifts, if the LHS is boolean it is treated as 0/1 i.e. do a shift, or not (instead of all 0s/all 1s for the bitwise ops)
macro_rules! eval_binary_shift {
    ($name:tt, $int_name:tt, $tag:expr) => {
        
        #[inline]
        pub fn $name (lhs: &Variant, rhs: &Variant) -> ExecResult<Variant> {
            if let Some(bit_value) = lhs.as_bits() {
                match rhs {
                    Variant::BoolFalse => return Ok(*lhs), // no-op, just copy the value to output
                    Variant::BoolTrue => return $int_name (bit_value, 1),
                    _ => if let Some(shift_value) = rhs.as_int() {
                        return $int_name (bit_value, shift_value)
                    }
                }
            }
            
            eval_meta_binary($tag, lhs, rhs)
        }
        
    };
}

eval_binary_shift!(eval_shl, int_shl, BinaryTag::Shl);
#[inline]
fn int_shl(lhs: IntType, rhs: IntType) -> ExecResult<Variant> { 
    if rhs < 0 { 
        return Err(ErrorKind::NegativeShiftCount.into()); 
    }
    checked_int_math!(checked_shl, lhs, rhs.try_into().unwrap()) 
}

eval_binary_shift!(eval_shr, int_shr, BinaryTag::Shr);
#[inline]
fn int_shr(lhs: IntType, rhs: IntType) -> ExecResult<Variant> {
    if rhs < 0 { 
        return Err(ErrorKind::NegativeShiftCount.into()); 
    }
    checked_int_math!(checked_shr, lhs, rhs.try_into().unwrap()) 
}
*/
