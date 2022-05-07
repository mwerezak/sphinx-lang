use crate::language::{IntType};
use crate::runtime::Variant;
use crate::runtime::strings::{StringValue, static_symbol};
use crate::runtime::types::{Type, MetaObject};
use crate::runtime::errors::{ExecResult};

// Booleans
impl MetaObject for bool {
    fn type_tag(&self) -> Type { Type::Boolean }
    
    fn as_bool(&self) -> ExecResult<bool> { Ok(*self) }
    
    fn as_bits(&self) -> Option<ExecResult<IntType>> {
        if *self { Some(Ok(!0)) } // all 1s
        else { Some(Ok(0)) } // all 0s
    }
    
    fn op_inv(&self) -> Option<ExecResult<Variant>> { 
        Some(Ok(Variant::from(!(*self)))) 
    }
    
    fn op_and(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(false))),
            Variant::BoolTrue => Some(Ok(Variant::from(*self))),
            _ => None,
        }
    }
    fn op_rand(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_and(lhs)
    }
    
    fn op_xor(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(*self))),
            Variant::BoolTrue => Some(Ok(Variant::from(!(*self)))),
            _ => None,
        }
    }
    fn op_rxor(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_xor(lhs)
    }
    
    fn op_or(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(*self))),
            Variant::BoolTrue => Some(Ok(Variant::from(true))),
            _ => None,
        }
    }
    fn op_ror(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_or(lhs)
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::BoolFalse => Some(Ok(!(*self))),
            Variant::BoolTrue => Some(Ok(*self)),
            _ => None,
        }
    }
    
    fn fmt_repr(&self) -> ExecResult<StringValue> {
        match self {
            true => Ok(static_symbol!("true").into()),
            false => Ok(static_symbol!("false").into()),
        }
    }
}