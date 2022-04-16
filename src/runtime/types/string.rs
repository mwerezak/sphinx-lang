use crate::runtime::Variant;
use crate::runtime::strings::StringValue;
use crate::runtime::types::{Type, MetaObject};
use crate::runtime::errors::{ExecResult};

// these trait bounds are weird but it allows us to drop the size of every Variant from 24 to 16 bytes
// as a StringValue cannot fit inside a Variant because of alignment. 
impl<T> MetaObject for T where T: Into<StringValue> + Copy, StringValue: From<T> {
    fn type_tag(&self) -> Type { Type::String }
    
    fn apply_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        if let Some(rhs) = rhs.as_strval() {
            return Some(StringValue::from(*self).concat(&rhs).map(|strval| Variant::from(strval)))
        }
        None
    }
    
    fn apply_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        if let Some(lhs) = lhs.as_strval() {
            return Some(lhs.concat(&(*self).into()).map(|strval| Variant::from(strval)))
        }
        None
    }
}