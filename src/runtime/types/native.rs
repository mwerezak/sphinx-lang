use core::any::Any;
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::types::{Type, MetaObject};


/// Trait for custom data
/// Currently unused as it requires trait upcasting
pub trait UserData: Any + GCTrace + MetaObject {
    fn type_tag(&self) -> Type { Type::UserData }
}