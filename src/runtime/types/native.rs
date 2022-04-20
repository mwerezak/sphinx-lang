use core::any::Any;
use core::ops::Deref;
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::types::{Type, MetaObject};


/// Trait for custom data
pub trait UserData: Any + GcTrace + MetaObject {
    fn type_tag(&self) -> Type { Type::UserData }
}