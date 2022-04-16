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
            
            (Variant::BoolTrue,  Variant::BoolTrue)  => Ok(true),
            (Variant::BoolFalse, Variant::BoolFalse) => Ok(true),
            (Variant::BoolTrue,  Variant::BoolFalse) => Ok(false),
            (Variant::BoolFalse, Variant::BoolTrue)  => Ok(false),
            
            (Variant::Integer(a), Variant::Integer(b)) => Ok(*a == *b),
            (Variant::Float(a), Variant::Float(b)) => Ok(*a == *b),
            
            (Variant::Tuple(a), Variant::Tuple(b)) 
                if a.is_empty() && b.is_empty() => Ok(true),
            
            _ => {
                if let (Some(a), Some(b)) = (self.as_strval(), other.as_strval()) {
                    return Ok(a == b);
                }
                
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
