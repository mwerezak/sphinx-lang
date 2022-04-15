use crate::runtime::strings::StringValue;
use crate::runtime::types::{Type, MetaObject};


impl MetaObject for StringValue {
    fn type_tag(&self) -> Type { Type::String }
}