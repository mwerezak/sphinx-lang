
use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::types::{MetaObject, Type};
use crate::runtime::errors::{ExecResult, ErrorKind, RuntimeError};

macro_rules! checked_int_math {
    ( $method:tt, $lhs:expr, $rhs:expr ) => {
        match $lhs.$method($rhs) {
            Some(value) => Ok(Variant::Integer(value)),
            None => Err(ErrorKind::OverflowError.into()),
        }
    };
}

impl MetaObject for IntType {
    fn type_tag(&self) -> Type { Type::Integer }
    
    fn as_bits(&self) -> Option<ExecResult<IntType>> { Some(Ok(*self)) }
    fn as_int(&self) -> Option<ExecResult<IntType>> { Some(Ok(*self)) }
    fn as_float(&self) -> Option<ExecResult<FloatType>> { Some(Ok(*self as FloatType)) }
    
    fn apply_neg(&self) -> Option<ExecResult<Variant>> { Some(Ok(Variant::from(-(*self)))) }
    fn apply_pos(&self) -> Option<ExecResult<Variant>> { Some(Ok(Variant::from(*self))) }
    fn apply_inv(&self) -> Option<ExecResult<Variant>> { Some(Ok(Variant::from(!(*self)))) }
    
    fn apply_mul(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_int().map(|rhs| checked_int_math!(checked_mul, *self, rhs?))
    }
    
    fn apply_rmul(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_mul(lhs)
    }
    
    fn apply_div(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_int().map(|rhs| {
            let rhs = rhs?;
            if rhs == 0 {
                Err(ErrorKind::DivideByZero.into())
            } else {
                checked_int_math!(checked_div, *self, rhs)
            }
        })
    }
    
    fn apply_rdiv(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_int().map(|lhs| {
            if *self == 0 {
                return Err(ErrorKind::DivideByZero.into());
            } else {
                checked_int_math!(checked_div, lhs?, *self)
            }
        })
    }
    
    fn apply_mod(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_int().map(|rhs| Ok(Variant::from(*self % rhs?)))
    }
    
    fn apply_rmod(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_int().map(|lhs| Ok(Variant::from(lhs? % *self)))
    }
    
    fn apply_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_int().map(|rhs| checked_int_math!(checked_add, *self, rhs?))
    }
    
    fn apply_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_add(lhs)
    }
    
    fn apply_sub(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_int().map(|rhs| checked_int_math!(checked_sub, *self, rhs?))
    }
    
    fn apply_rsub(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_int().map(|lhs| checked_int_math!(checked_sub, lhs?, *self))
    }
}


impl MetaObject for FloatType {
    fn type_tag(&self) -> Type { Type::Float }
    
    fn as_float(&self) -> Option<ExecResult<FloatType>> { Some(Ok(*self)) }
    
    fn apply_neg(&self) -> Option<ExecResult<Variant>> { Some(Ok(Variant::from(-(*self)))) }
    fn apply_pos(&self) -> Option<ExecResult<Variant>> { Some(Ok(Variant::from(*self))) }
    
    fn apply_mul(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_float().map(|rhs| Ok(Variant::from(*self * rhs?)))
    }
    
    fn apply_rmul(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_mul(lhs)
    }
    
    fn apply_div(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_float().map(|rhs| Ok(Variant::from(*self / rhs?)))
    }
    
    fn apply_rdiv(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_float().map(|lhs| Ok(Variant::from(lhs? / *self)))
    }
    
    fn apply_mod(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_float().map(|rhs| Ok(Variant::from(*self % rhs?)))
    }
    
    fn apply_rmod(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_float().map(|lhs| Ok(Variant::from(lhs? % *self)))
    }
    
    fn apply_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_float().map(|rhs| Ok(Variant::from(*self + rhs?)))
    }
    
    fn apply_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_add(lhs)
    }
    
    fn apply_sub(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_float().map(|rhs| Ok(Variant::from(*self - rhs?)))
    }
    
    fn apply_rsub(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_float().map(|lhs| Ok(Variant::from(lhs? - *self)))
    }
}

