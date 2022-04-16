
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
    
    fn op_neg(&self) -> Option<ExecResult<Variant>> { Some(Ok(Variant::from(-(*self)))) }
    fn op_pos(&self) -> Option<ExecResult<Variant>> { Some(Ok(Variant::from(*self))) }
    fn op_inv(&self) -> Option<ExecResult<Variant>> { Some(Ok(Variant::from(!(*self)))) }
    
    fn op_mul(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::Integer(rhs) => Some(checked_int_math!(checked_mul, *self, *rhs)),
            _ => rhs.as_meta().as_int()
                .map(|rhs| checked_int_math!(checked_mul, *self, rhs?))
        }
    }
    
    fn op_rmul(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_mul(lhs)
    }
    
    fn op_div(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_int().map(|rhs| {
            let rhs = rhs?;
            if rhs == 0 {
                Err(ErrorKind::DivideByZero.into())
            } else {
                checked_int_math!(checked_div, *self, rhs)
            }
        })
    }
    
    fn op_rdiv(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_int().map(|lhs| {
            if *self == 0 {
                return Err(ErrorKind::DivideByZero.into());
            } else {
                checked_int_math!(checked_div, lhs?, *self)
            }
        })
    }
    
    fn op_mod(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_int().map(|rhs| Ok(Variant::from(*self % rhs?)))
    }
    
    fn op_rmod(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_int().map(|lhs| Ok(Variant::from(lhs? % *self)))
    }
    
    fn op_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::Integer(rhs) => Some(checked_int_math!(checked_add, *self, *rhs)),
            _ => rhs.as_meta().as_int()
                .map(|rhs| checked_int_math!(checked_add, *self, rhs?))
        }
    }
    
    fn op_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_add(lhs)
    }
    
    fn op_sub(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::Integer(rhs) => Some(checked_int_math!(checked_sub, *self, *rhs)),
            _ => rhs.as_meta().as_int()
                .map(|rhs| checked_int_math!(checked_sub, *self, rhs?))
        }
    }
    
    fn op_rsub(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        match lhs {
            Variant::Integer(lhs) => Some(checked_int_math!(checked_sub, *lhs, *self)),
            _ => lhs.as_meta().as_int()
                .map(|lhs| checked_int_math!(checked_sub, lhs?, *self))
        }
    }
    
    fn op_and(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        let lhs = self.as_bits().unwrap().unwrap();
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(lhs & false.as_bits().unwrap().unwrap()))),
            Variant::BoolTrue => Some(Ok(Variant::from(lhs & true.as_bits().unwrap().unwrap()))),
            Variant::Integer(rhs) => Some(Ok(Variant::from(lhs & rhs.as_bits().unwrap().unwrap()))),
            _ => rhs.as_meta().as_bits()
                .map(|rhs| Ok(Variant::from(lhs & rhs?)))
        }
    }
    
    fn op_rand(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_and(lhs)
    }

    fn op_xor(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        let lhs = self.as_bits().unwrap().unwrap();
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(lhs ^ false.as_bits().unwrap().unwrap()))),
            Variant::BoolTrue => Some(Ok(Variant::from(lhs ^ true.as_bits().unwrap().unwrap()))),
            Variant::Integer(rhs) => Some(Ok(Variant::from(lhs ^ rhs.as_bits().unwrap().unwrap()))),
            _ => rhs.as_meta().as_bits()
                .map(|rhs| Ok(Variant::from(lhs ^ rhs?)))
        }
    }
    
    fn op_rxor(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_xor(lhs)
    }

    fn op_or(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        let lhs = self.as_bits().unwrap().unwrap();
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(lhs | false.as_bits().unwrap().unwrap()))),
            Variant::BoolTrue => Some(Ok(Variant::from(lhs | true.as_bits().unwrap().unwrap()))),
            Variant::Integer(rhs) => Some(Ok(Variant::from(lhs | rhs.as_bits().unwrap().unwrap()))),
            _ => rhs.as_meta().as_bits()
                .map(|rhs| Ok(Variant::from(lhs | rhs?)))
        }
    }
    
    fn op_ror(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_or(lhs)
    }
    
    fn op_shl(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        let rhs = match rhs {
            Variant::BoolFalse => Some(0),
            Variant::BoolTrue => Some(1),
            Variant::Integer(rhs) => Some(*rhs),
            _ => match rhs.as_meta().as_int() {
                Some(Ok(rhs)) => Some(rhs),
                Some(Err(error)) => return Some(Err(error)),
                None => None,
            },
        };
        
        rhs.map(|rhs| {
            if rhs < 0 {
                return Err(ErrorKind::NegativeShiftCount.into());
            }
            return checked_int_math!(checked_shl, *self, rhs.try_into().unwrap());
        })
    }
    
    fn op_rshl(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_bits().map(|lhs| {
            if lhs.is_err() {
                return Err(lhs.unwrap_err());
            }
            if *self < 0 {
                return Err(ErrorKind::NegativeShiftCount.into());
            }
            return checked_int_math!(checked_shl, lhs.unwrap(), (*self).try_into().unwrap());
        })
    }
    
    fn op_shr(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        let rhs = match rhs {
            Variant::BoolFalse => Some(0),
            Variant::BoolTrue => Some(1),
            Variant::Integer(rhs) => Some(*rhs),
            _ => match rhs.as_meta().as_int() {
                Some(Ok(rhs)) => Some(rhs),
                Some(Err(error)) => return Some(Err(error)),
                None => None,
            },
        };
        
        rhs.map(|rhs| {
            if rhs < 0 {
                return Err(ErrorKind::NegativeShiftCount.into());
            }
            return checked_int_math!(checked_shr, *self, rhs.try_into().unwrap());
        })
    }
    
    fn op_rshr(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_bits().map(|lhs| {
            if lhs.is_err() {
                return Err(lhs.unwrap_err());
            }
            if *self < 0 {
                return Err(ErrorKind::NegativeShiftCount.into());
            }
            return checked_int_math!(checked_shr, lhs.unwrap(), (*self).try_into().unwrap());
        })
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Integer(other) => Some(Ok(*self == *other)),
            _ => other.as_meta().as_int()
                .map(|other| Ok(*self == other?))
        }
    }
    
    fn cmp_lt(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Integer(other) => Some(Ok(*self < *other)),
            _ => other.as_meta().as_int()
                .map(|other| Ok(*self < other?))
        }
    }
    
    fn cmp_le(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Integer(other) => Some(Ok(*self <= *other)),
            _ => other.as_meta().as_int()
                .map(|other| Ok(*self <= other?))
        }
    }
}


impl MetaObject for FloatType {
    fn type_tag(&self) -> Type { Type::Float }
    
    fn as_float(&self) -> Option<ExecResult<FloatType>> { Some(Ok(*self)) }
    
    fn op_neg(&self) -> Option<ExecResult<Variant>> { Some(Ok(Variant::from(-(*self)))) }
    fn op_pos(&self) -> Option<ExecResult<Variant>> { Some(Ok(Variant::from(*self))) }
    
    fn op_mul(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_float().map(|rhs| Ok(Variant::from(*self * rhs?)))
    }
    
    fn op_rmul(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_mul(lhs)
    }
    
    fn op_div(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_float().map(|rhs| Ok(Variant::from(*self / rhs?)))
    }
    
    fn op_rdiv(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_float().map(|lhs| Ok(Variant::from(lhs? / *self)))
    }
    
    fn op_mod(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_float().map(|rhs| Ok(Variant::from(*self % rhs?)))
    }
    
    fn op_rmod(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_float().map(|lhs| Ok(Variant::from(lhs? % *self)))
    }
    
    fn op_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_float().map(|rhs| Ok(Variant::from(*self + rhs?)))
    }
    
    fn op_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_add(lhs)
    }
    
    fn op_sub(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        rhs.as_meta().as_float().map(|rhs| Ok(Variant::from(*self - rhs?)))
    }
    
    fn op_rsub(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        lhs.as_meta().as_float().map(|lhs| Ok(Variant::from(lhs? - *self)))
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        other.as_meta().as_float().map(|other| Ok(*self == other?))
    }
    
    fn cmp_lt(&self, other: &Variant) -> Option<ExecResult<bool>> {
        other.as_meta().as_float().map(|other| Ok(*self < other?))
    }
    
    fn cmp_le(&self, other: &Variant) -> Option<ExecResult<bool>> {
        other.as_meta().as_float().map(|other| Ok(*self <= other?))
    }
}

