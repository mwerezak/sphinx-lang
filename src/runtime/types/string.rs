use core::fmt::Write;
use crate::runtime::Variant;
use crate::runtime::strings::{StringValue, StrBuffer};
use crate::runtime::types::{Type, MetaObject};
use crate::runtime::errors::{ExecResult};


impl MetaObject for StringValue {
    fn type_tag(&self) -> Type { Type::String }
    
    fn len(&self) -> Option<ExecResult<usize>> {
        Some(Ok(self.char_count()))
    }
    
    fn op_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        if let Some(rhs) = rhs.as_strval() {
            return Some(self.concat(&rhs).map(Variant::from))
        }
        None
    }
    
    fn op_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        if let Some(lhs) = lhs.as_strval() {
            return Some(lhs.concat(self).map(Variant::from))
        }
        None
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        if let Some(other) = other.as_strval() {
            return Some(Ok(*self == other))
        }
        None
    }
    
    fn cmp_lt(&self, other: &Variant) -> Option<ExecResult<bool>> {
        if let Some(other) = other.as_strval() {
            return Some(Ok(*self < other))
        }
        None
    }
    
    fn cmp_le(&self, other: &Variant) -> Option<ExecResult<bool>> {
        if let Some(other) = other.as_strval() {
            return Some(Ok(*self <= other))
        }
        None
    }
    
    fn fmt_echo(&self) -> ExecResult<StringValue> {
        let mut buf = StrBuffer::<64>::new();
        if write!(buf, "\"{}\"", self).is_ok() {
            Ok(StringValue::new_uninterned(buf))
        } else {
            // resort to allocated buffer
            Ok(StringValue::new_uninterned(format!("\"{}\"", self)))
        }
    }
}