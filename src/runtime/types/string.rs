use crate::runtime::Variant;
use crate::runtime::strings::StringValue;
use crate::runtime::types::{Type, MetaObject};
use crate::runtime::errors::{ExecResult};


impl MetaObject for StringValue {
    fn type_tag(&self) -> Type { Type::String }
    
    fn apply_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        if let Variant::String(rhs) = rhs {
            return Some(self.concat(rhs).map(|strval| Variant::from(strval)))
        }
        None
    }
    
    fn apply_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        if let Variant::String(lhs) = lhs {
            return Some(lhs.concat(self).map(|strval| Variant::from(strval)))
        }
        None
    }
}