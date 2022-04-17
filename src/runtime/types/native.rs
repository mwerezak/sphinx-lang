use core::any::Any;
use core::ops::Deref;
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::types::{Type, MetaObject};


/// Variant can't hold a fat pointer as it's only 16 bytes wide and
/// there has to be space for the enum discriminant
/// So this is used to avoid that, at the cost of a double-indirection
/// (which can be avoided once DST support is stabilized)
pub struct UserDataBox {
    data: Box<dyn UserData>,
}

impl Deref for UserDataBox {
    type Target = dyn UserData;
    fn deref(&self) -> &Self::Target {
        &*self.data
    }
}

unsafe impl GCTrace for UserDataBox {
    fn trace(&self) {
        self.data.trace()
    }
    
    fn size_hint(&self) -> usize {
        self.data.size_hint()
    }
}


/// Trait for custom data
pub trait UserData: Any + GCTrace + MetaObject {
    fn type_tag(&self) -> Type { Type::UserData }
}