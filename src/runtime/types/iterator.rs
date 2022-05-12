use crate::runtime::Variant;
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::strings::StringValue;
use crate::runtime::types::{Type, MetaObject};
use crate::runtime::iter::IterState;
use crate::runtime::errors::{ExecResult};


/// Similar use case as UserData but a bit more limited in scope
pub trait UserIterator: GcTrace {
    fn next_state(&self, state: Option<&Variant>) -> ExecResult<Variant>;
    fn get_item(&self, state: &Variant) -> ExecResult<Variant>;
}

// unlike UserData, the MetaObject impl for UserIterator is not customizable
impl MetaObject for Gc<dyn UserIterator> {
    fn type_tag(&self) -> Type { Type::Iterator }
    
    fn fmt_repr(&self) -> ExecResult<StringValue> {
        let result = format!(
            "<{} at {:#X}>", self.type_name()?, Gc::as_id(self)
        );
        
        Ok(StringValue::new_uninterned(result))
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Iterator(other) => Some(Ok(Gc::ptr_eq(self, other))),
            _ => Some(Ok(false)),
        }
    }
    
    fn iter_get(&self, state: &Variant) -> Option<ExecResult<Variant>> {
        Some(self.get_item(state))
    }
    
    fn iter_next(&self, state: &Variant) -> Option<ExecResult<Variant>> {
        Some(self.next_state(Some(state)))
    }
    
    fn iter_init(&self) -> Option<ExecResult<IterState>> {
        let state = match self.next_state(None) {
            Ok(state) => state,
            Err(error) => return Some(Err(error)),
        };
        
        let iter = Variant::Iterator(*self);
        Some(Ok(IterState::new(iter, state)))
    }
}
