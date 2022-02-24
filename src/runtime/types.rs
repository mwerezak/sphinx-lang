use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::runtime::data::{Variant, InternStr};
use crate::runtime::errors::RuntimeResult;


// Type Object

pub struct RuntimeType {
    name: InternStr,
    slots: SlotMetatable,
}

impl RuntimeType {
    pub fn name(&self) -> InternStr { self.name }
    pub fn slots(&self) -> &SlotMetatable { &self.slots }
}



// Container of function pointers used to implement metamethods

type SlotUnaryOp = fn(Variant) -> RuntimeResult<Variant>;
type SlotBinaryOp = fn(Variant, Variant) -> RuntimeResult<Variant>;
type SlotComparison = fn(Variant, Variant) -> RuntimeResult<bool>;

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
