use core::str::FromStr;
use core::fmt::{self, Write};
use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::strings::{StringValue, StrBuffer};
use crate::runtime::types::{MetaObject, Type};
use crate::runtime::errors::{ExecResult, ErrorKind};

macro_rules! checked_int_math {
    ( $method:tt, $lhs:expr, $rhs:expr ) => {
        match $lhs.$method($rhs) {
            Some(value) => Ok(Variant::Integer(value)),
            None => Err(ErrorKind::OverflowError.into()),
        }
    };
}

pub fn int_from_str(s: &str, radix: IntType) -> ExecResult<IntType> {
    if !(2..=36).contains(&radix) {
        return Err(ErrorKind::StaticMessage("invalid radix").into());
    }
    
    let value = IntType::from_str_radix(s, radix.try_into().unwrap())
        .map_err(|_| ErrorKind::Message(format!("could not parse \"{}\" as int with radix {}", s, radix)))?;
    Ok(value)
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
                Err(ErrorKind::DivideByZero.into())
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
            let lhs = lhs?;
            if *self < 0 {
                return Err(ErrorKind::NegativeShiftCount.into());
            }
            return checked_int_math!(checked_shl, lhs, (*self).try_into().unwrap());
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
            let lhs = lhs?;
            if *self < 0 {
                return Err(ErrorKind::NegativeShiftCount.into());
            }
            return checked_int_math!(checked_shr, lhs, (*self).try_into().unwrap());
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
    
    fn fmt_echo(&self) -> ExecResult<StringValue> {
        let mut buf = StrBuffer::<32>::new();
        if write!(buf, "{}", *self).is_ok() {
            Ok(StringValue::new_maybe_interned(buf))
        } else {
            // resort to allocated buffer
            Ok(StringValue::new_maybe_interned(format!("{}", *self)))
        }
    }
}


pub fn float_from_str(s: &str) -> ExecResult<FloatType> {
    let value = FloatType::from_str(s)
        .map_err(|_| ErrorKind::Message(format!("could not parse \"{}\" as float", s)))?;
    Ok(value)
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
    
    fn fmt_echo(&self) -> ExecResult<StringValue> {
        fn write_float(value: FloatType, fmt: &mut impl fmt::Write) -> fmt::Result {
            if !value.is_finite() || value.trunc() != value {
                write!(fmt, "{}", value)
            } else {
                write!(fmt, "{}.0", value)
            }
        }
        
        let mut buf = StrBuffer::<32>::new();
        if write_float(*self, &mut buf).is_ok() {
            Ok(StringValue::new_maybe_interned(buf))
        } else {
            let mut buf = String::new();
            write_float(*self, &mut buf).map_err(ErrorKind::from)?;
            Ok(StringValue::new_maybe_interned(buf))
        }
    }
}

