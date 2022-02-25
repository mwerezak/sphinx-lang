pub mod builder;
pub mod primitive;

use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::runtime::data::{Variant, InternStr};
use crate::runtime::errors::RuntimeResult;


// Type Object

pub type TypeID = InternStr;

pub struct RuntimeType {
    name: InternStr,
    type_id: TypeID, // used to uniquely identify the type
    
    slots: SlotMetatable,
}

impl RuntimeType {
    pub fn name(&self) -> InternStr { self.name }
    pub fn type_id(&self) -> TypeID { self.type_id }
    
    pub fn slots(&self) -> &SlotMetatable { &self.slots }
    pub fn slots_mut(&mut self) -> &mut SlotMetatable { &mut self.slots }
}

// Container of function pointers used to implement metamethods

type SlotUnaryOp = fn(Variant) -> RuntimeResult<Variant>;
type SlotBinaryOp = fn(Variant, Variant) -> RuntimeResult<Option<Variant>>;
type SlotComparison = fn(Variant, Variant) -> RuntimeResult<Option<bool>>;

type Slot<T> = fn(Variant) -> RuntimeResult<T>;

// TODO store slots in an array instead and lookup by enum?
// would make it much easier to implement slot-transforming helper functions

#[derive(Default)]
pub struct SlotMetatable {
    // Special runtime slots
    
    // __bool
    // __tostring
    // __call
    
    // Member access and Indexing
    
    // If these produce a descriptor, then attr/index acts as RW
    // If they produce any other value, then attr/index acts as RO (produce error if del or assignment was used)
    // TODO descriptors
    
    // __attr
    // __index
    
    // Comparisons
    
    pub eq: Option<SlotComparison>,
    pub lt: Option<SlotComparison>,
    pub le: Option<SlotComparison>,
    
    // Numeric operations
    
    pub neg: Option<SlotUnaryOp>,  // __neg
    pub pos: Option<SlotUnaryOp>,  // __pos
    pub inv: Option<SlotUnaryOp>,  // __inv
    
    // most likely if we don't have metamethods for reflected versions 
    // of binops users will just have to implement them themselves
    pub mul:  Option<SlotBinaryOp>, // __mul
    pub rmul: Option<SlotBinaryOp>, // __rmul
    pub div:  Option<SlotBinaryOp>, // __div
    pub rdiv: Option<SlotBinaryOp>, // __rdiv
    pub mod_: Option<SlotBinaryOp>, // __mod
    pub rmod: Option<SlotBinaryOp>, // __rmod
    pub add:  Option<SlotBinaryOp>, // __add
    pub radd: Option<SlotBinaryOp>, // __radd
    pub sub:  Option<SlotBinaryOp>, // __sub
    pub rsub: Option<SlotBinaryOp>, // __rsub
    pub shl:  Option<SlotBinaryOp>, // __lshift
    pub rshl: Option<SlotBinaryOp>, // __lshrift
    pub shr:  Option<SlotBinaryOp>, // __rshift
    pub rshr: Option<SlotBinaryOp>, // __rshrift
    pub and:  Option<SlotBinaryOp>, // __and
    pub rand: Option<SlotBinaryOp>, // __rand
    pub xor:  Option<SlotBinaryOp>, // __xor
    pub rxor: Option<SlotBinaryOp>, // __rxor
    pub or:   Option<SlotBinaryOp>, // __or
    pub ror:  Option<SlotBinaryOp>, // __ror
}

// Notes: primitive types should use slots directly
// When metamethods are assigned to on user defined types (i.e. classes),
// a special slot should be assigned here that accesses the corresponding "__" attribute 
// on the object and uses that to produce the result

// Possible future plan, User classes could have a separate "class" metatable, 
// to customize behaviour of the class itself,


// These methods call the corresponding slot and return either Some(result) or None if the slot is None:

macro_rules! fn_call_slot_unary_op {
    ( $slot:tt, $call:tt ) => {
        
        pub fn $call(&self, operand: Variant) -> Option<RuntimeResult<Variant>> {
            self.$slot.map(|$slot| $slot(operand))
        }
        
    };
}

macro_rules! fn_call_slot_binary_op {
    ( $slot:tt, $call:tt ) => {
        
        // Produce None if the operation was not supported by the type
        // i.e. if either the slot was None or the slot function returned None
        pub fn $call(&self, lhs: Variant, rhs: Variant) -> Option<RuntimeResult<Variant>> {
            match self.$slot {
                None => None,
                
                Some($slot) => match $slot(lhs, rhs) {
                    Ok(result) => result.map(|value| Ok(value)),
                    Err(error) => Some(Err(error)),
                },
            }
        }
        
    };
}

macro_rules! fn_call_slot_comparison {
    ( $slot:tt, $call:tt ) => {
        
        // Produce None if the operation was not supported by the type
        // i.e. if either the slot was None or the slot function returned None
        pub fn $call(&self, lhs: Variant, rhs: Variant) -> Option<RuntimeResult<bool>> {
            match self.$slot {
                None => None,
                
                Some($slot) => match $slot(lhs, rhs) {
                    Ok(result) => result.map(|value| Ok(value)),
                    Err(error) => Some(Err(error)),
                },
            }
        }
        
    };
}

macro_rules! fn_has_slot {
    ( $slot:tt, $has:tt ) => {
        pub fn $has(&self) -> bool { self.$slot.is_some() }
    };
}

// I've considered having ways to lookup slots using a value, like an enum
// but slot types are heterogenous anyways so it wouldn't add that much value

impl SlotMetatable {
    // Comparison
    
    fn_has_slot!(eq, has_eq);
    fn_has_slot!(lt, has_lt);
    fn_has_slot!(le, has_le);
    
    fn_call_slot_comparison!(eq, call_eq);
    fn_call_slot_comparison!(lt, call_lt);
    fn_call_slot_comparison!(le, call_le);
    
    // Unary
    
    fn_has_slot!(neg, has_neg);
    fn_has_slot!(pos, has_pos);
    fn_has_slot!(inv, has_inv);
    
    fn_call_slot_unary_op!(neg, call_neg);
    fn_call_slot_unary_op!(pos, call_pos);
    fn_call_slot_unary_op!(inv, call_inv);
    
    // Binary
    
    fn_has_slot!(mul,  has_mul);
    fn_has_slot!(rmul, has_rmul);
    fn_has_slot!(div,  has_div);
    fn_has_slot!(rdiv, has_rdiv);
    fn_has_slot!(mod_, has_mod);
    fn_has_slot!(rmod, has_rmod);
    fn_has_slot!(add,  has_add);
    fn_has_slot!(radd, has_radd);
    fn_has_slot!(sub,  has_sub);
    fn_has_slot!(rsub, has_rsub);
    fn_has_slot!(shl,  has_shl);
    fn_has_slot!(rshl, has_rshl);
    fn_has_slot!(shr,  has_shr);
    fn_has_slot!(rshr, has_rshr);
    fn_has_slot!(and,  has_and);
    fn_has_slot!(rand, has_rand);
    fn_has_slot!(xor,  has_xor);
    fn_has_slot!(rxor, has_rxor);
    fn_has_slot!(or,   has_or);
    fn_has_slot!(ror,  has_ror);
    
    fn_call_slot_binary_op!(mul,  call_mul);
    fn_call_slot_binary_op!(rmul, call_rmul);
    fn_call_slot_binary_op!(div,  call_div);
    fn_call_slot_binary_op!(rdiv, call_rdiv);
    fn_call_slot_binary_op!(mod_, call_mod);
    fn_call_slot_binary_op!(rmod, call_rmod);
    fn_call_slot_binary_op!(add,  call_add);
    fn_call_slot_binary_op!(radd, call_radd);
    fn_call_slot_binary_op!(sub,  call_sub);
    fn_call_slot_binary_op!(rsub, call_rsub);
    fn_call_slot_binary_op!(shl,  call_shl);
    fn_call_slot_binary_op!(rshl, call_rshl);
    fn_call_slot_binary_op!(shr,  call_shr);
    fn_call_slot_binary_op!(rshr, call_rshr);
    fn_call_slot_binary_op!(and,  call_and);
    fn_call_slot_binary_op!(rand, call_rand);
    fn_call_slot_binary_op!(xor,  call_xor);
    fn_call_slot_binary_op!(rxor, call_rxor);
    fn_call_slot_binary_op!(or,   call_or);
    fn_call_slot_binary_op!(ror,  call_ror);
}