
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
        match rhs {
            Variant::Integer(rhs) => Some(checked_int_math!(checked_mul, *self, *rhs)),
            _ => rhs.as_meta().as_int()
                .map(|rhs| checked_int_math!(checked_mul, *self, rhs?))
        }
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
        match rhs {
            Variant::Integer(rhs) => Some(checked_int_math!(checked_add, *self, *rhs)),
            _ => rhs.as_meta().as_int()
                .map(|rhs| checked_int_math!(checked_add, *self, rhs?))
        }
    }
    
    fn apply_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_add(lhs)
    }
    
    fn apply_sub(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::Integer(rhs) => Some(checked_int_math!(checked_sub, *self, *rhs)),
            _ => rhs.as_meta().as_int()
                .map(|rhs| checked_int_math!(checked_sub, *self, rhs?))
        }
    }
    
    fn apply_rsub(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        match lhs {
            Variant::Integer(lhs) => Some(checked_int_math!(checked_sub, *lhs, *self)),
            _ => lhs.as_meta().as_int()
                .map(|lhs| checked_int_math!(checked_sub, lhs?, *self))
        }
    }
    
    fn apply_and(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        let lhs = self.as_bits().unwrap().unwrap();
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(lhs & false.as_bits().unwrap().unwrap()))),
            Variant::BoolTrue => Some(Ok(Variant::from(lhs & true.as_bits().unwrap().unwrap()))),
            Variant::Integer(rhs) => Some(Ok(Variant::from(lhs & rhs.as_bits().unwrap().unwrap()))),
            _ => rhs.as_meta().as_bits()
                .map(|rhs| Ok(Variant::from(lhs & rhs?)))
        }
    }
    
    fn apply_rand(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_and(lhs)
    }

    fn apply_xor(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        let lhs = self.as_bits().unwrap().unwrap();
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(lhs ^ false.as_bits().unwrap().unwrap()))),
            Variant::BoolTrue => Some(Ok(Variant::from(lhs ^ true.as_bits().unwrap().unwrap()))),
            Variant::Integer(rhs) => Some(Ok(Variant::from(lhs ^ rhs.as_bits().unwrap().unwrap()))),
            _ => rhs.as_meta().as_bits()
                .map(|rhs| Ok(Variant::from(lhs ^ rhs?)))
        }
    }
    
    fn apply_rxor(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_xor(lhs)
    }

    fn apply_or(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        let lhs = self.as_bits().unwrap().unwrap();
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(lhs | false.as_bits().unwrap().unwrap()))),
            Variant::BoolTrue => Some(Ok(Variant::from(lhs | true.as_bits().unwrap().unwrap()))),
            Variant::Integer(rhs) => Some(Ok(Variant::from(lhs | rhs.as_bits().unwrap().unwrap()))),
            _ => rhs.as_meta().as_bits()
                .map(|rhs| Ok(Variant::from(lhs | rhs?)))
        }
    }
    
    fn apply_ror(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_or(lhs)
    }
    
    fn apply_shl(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
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
    
    fn apply_rshl(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
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
    
    fn apply_shr(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
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
    
    fn apply_rshr(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
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

