//! Binary and unary operations with certain primitive types will *short-circuit*,
//! meaning that the resulting value will be computed using the logic defined here
//! instead of deferring to the type system.
//!
//! For most functions in this module, returning None means that the operation
//! should be deferred to the metatables of the operands involved.

use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::gc::Gc;
use crate::runtime::types::MetaObject;
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
        meta_eval_unary!(self, op_neg)
    }
    
    #[inline(always)]
    pub fn apply_pos(&self) -> ExecResult<Variant> {
        meta_eval_unary!(self, op_pos)
    }
    
    #[inline(always)]
    pub fn apply_inv(&self) -> ExecResult<Variant> {
        meta_eval_unary!(self, op_inv)
    }
    
    #[inline(always)]
    pub fn apply_not(&self) -> ExecResult<Variant> {
        Ok(Variant::from(!self.as_bool()?))
    }
    
    // Arithmetic
    
    #[inline(always)]
    pub fn apply_mul(&self, rhs: &Variant) -> ExecResult<Variant> {
        meta_eval_binary!(self, rhs, op_mul, op_rmul)
    }
    
    #[inline(always)]
    pub fn apply_div(&self, rhs: &Variant) -> ExecResult<Variant> {
        meta_eval_binary!(self, rhs, op_div, op_rdiv)
    }
    
    #[inline(always)]
    pub fn apply_mod(&self, rhs: &Variant) -> ExecResult<Variant> {
        meta_eval_binary!(self, rhs, op_mod, op_rmod)
    }
    
    #[inline(always)]
    pub fn apply_add(&self, rhs: &Variant) -> ExecResult<Variant> {
        meta_eval_binary!(self, rhs, op_add, op_radd)
    }
    
    #[inline(always)]
    pub fn apply_sub(&self, rhs: &Variant) -> ExecResult<Variant> {
        meta_eval_binary!(self, rhs, op_sub, op_rsub)
    }
    
    // Bitwise
    
    pub fn apply_and(&self, rhs: &Variant) -> ExecResult<Variant> {
        meta_eval_binary!(self, rhs, op_and, op_rand)
    }
    
    pub fn apply_xor(&self, rhs: &Variant) -> ExecResult<Variant> {
        meta_eval_binary!(self, rhs, op_xor, op_rxor)
    }
    
    pub fn apply_or(&self, rhs: &Variant) -> ExecResult<Variant> {
        meta_eval_binary!(self, rhs, op_or, op_ror)
    }
    
    // Shifts
    
    pub fn apply_shl(&self, rhs: &Variant) -> ExecResult<Variant> {
        meta_eval_binary!(self, rhs, op_shl, op_rshl)
    }
    
    pub fn apply_shr(&self, rhs: &Variant) -> ExecResult<Variant> {
        meta_eval_binary!(self, rhs, op_shr, op_rshr)
    }

    // Comparison
    
    pub fn cmp_eq(&self, other: &Variant) -> ExecResult<bool> {
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
    
    pub fn cmp_ne(&self, other: &Variant) -> ExecResult<bool> {
        self.cmp_eq(other).map(|cmp| !cmp)
    }
    
    pub fn cmp_lt(&self, other: &Variant) -> ExecResult<bool> {
        meta_eval_inequality!(self, other, cmp_lt, cmp_le)
    }
    
    pub fn cmp_le(&self, other: &Variant) -> ExecResult<bool> {
        meta_eval_inequality!(self, other, cmp_le, cmp_lt)
    }
    
    pub fn cmp_gt(&self, other: &Variant) -> ExecResult<bool> {
        self.cmp_le(other).map(|cmp| !cmp)
    }

    pub fn cmp_ge(&self, other: &Variant) -> ExecResult<bool> {
        self.cmp_lt(other).map(|cmp| !cmp)
    }
}
