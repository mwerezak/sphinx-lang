use crate::runtime::Variant;
use crate::runtime::strings::StringValue;
use crate::runtime::types::{Type, MetaObject};
use crate::runtime::errors::{ExecResult};


impl MetaObject for StringValue {
    fn type_tag(&self) -> Type { Type::String }
    
    fn op_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        if let Some(rhs) = rhs.as_strval() {
            return Some(self.concat(&rhs).map(|strval| Variant::from(strval)))
        }
        None
    }
    
    fn op_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        if let Some(lhs) = lhs.as_strval() {
            return Some(lhs.concat(self).map(|strval| Variant::from(strval)))
        }
        None
    }
}