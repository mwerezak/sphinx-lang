use crate::runtime::Runtime;
use crate::runtime::types::{RuntimeType, SlotMetatable};
use crate::runtime::types::{SlotBinaryOp, SlotUnaryOp};
use crate::runtime::errors::{RuntimeResult, RuntimeError, ErrorKind};

pub struct RuntimeTypeBuilder {
    name: String,
    type_id: String,
    slots: SlotMetatable,
}

// set_slot() and clear_slot()
macro_rules! fn_update_slot {
    ( $slot:tt, $stype:ty, $set_name:tt, $clear_name:tt ) => {
        pub fn $set_name(mut self, func: $stype) -> Self {
            self.slots.$slot = Some(func);
            self
        }
        
        pub fn $clear_name(mut self) -> Self {
            self.slots.$slot = None;
            self
        }
    };
}

impl RuntimeTypeBuilder {
    pub fn new<S: ToString>(name: S, type_id: S) -> Self {
        RuntimeTypeBuilder {
            name: name.to_string(),
            type_id: type_id.to_string(),
            slots: SlotMetatable::default(),
        }
    }
    
    pub fn build(self, runtime: &mut Runtime) -> RuntimeResult<&mut RuntimeType> {
        let name = runtime.get_or_intern_str(self.name);
        let type_id = runtime.get_or_intern_str(self.type_id);
        
        let rtype = RuntimeType {
            name, type_id, slots: self.slots,
        };
        runtime.register_type(type_id, rtype)
    }
    
    fn_update_slot!(neg, SlotUnaryOp, set_neg, clear_neg);
    fn_update_slot!(pos, SlotUnaryOp, set_pos, clear_pos);
    fn_update_slot!(inv, SlotUnaryOp, set_inv, clear_inv);
    
    fn_update_slot!(mul,  SlotBinaryOp, set_mul,  clear_mul);
    fn_update_slot!(rmul, SlotBinaryOp, set_rmul, clear_rmul);
    fn_update_slot!(div,  SlotBinaryOp, set_div,  clear_div);
    fn_update_slot!(rdiv, SlotBinaryOp, set_rdiv, clear_rdiv);
    fn_update_slot!(mod_, SlotBinaryOp, set_mod,  clear_mod);
    fn_update_slot!(rmod, SlotBinaryOp, set_rmod, clear_rmod);
    fn_update_slot!(add,  SlotBinaryOp, set_add,  clear_add);
    fn_update_slot!(radd, SlotBinaryOp, set_radd, clear_radd);
    fn_update_slot!(sub,  SlotBinaryOp, set_sub,  clear_sub);
    fn_update_slot!(rsub, SlotBinaryOp, set_rsub, clear_rsub);
    fn_update_slot!(shl,  SlotBinaryOp, set_shl,  clear_shl);
    fn_update_slot!(rshl, SlotBinaryOp, set_rshl, clear_rshl);
    fn_update_slot!(shr,  SlotBinaryOp, set_shr,  clear_shr);
    fn_update_slot!(rshr, SlotBinaryOp, set_rshr, clear_rshr);
    fn_update_slot!(and,  SlotBinaryOp, set_and,  clear_and);
    fn_update_slot!(rand, SlotBinaryOp, set_rand, clear_rand);
    fn_update_slot!(xor,  SlotBinaryOp, set_xor,  clear_xor);
    fn_update_slot!(rxor, SlotBinaryOp, set_rxor, clear_rxor);
    fn_update_slot!(or,   SlotBinaryOp, set_or,   clear_or);
    fn_update_slot!(ror,  SlotBinaryOp, set_ror,  clear_ror);
    
    fn_update_slot!(eq, SlotBinaryOp, set_eq, clear_eq);
    fn_update_slot!(lt, SlotBinaryOp, set_lt, clear_lt);
    fn_update_slot!(le, SlotBinaryOp, set_le, clear_le);
}