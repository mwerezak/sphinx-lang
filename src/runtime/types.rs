use core::fmt;
use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::iter::IterState;
use crate::runtime::function::Call;
use crate::runtime::strings::{StringValue, static_symbol};
use crate::runtime::errors::{ExecResult, RuntimeError};


mod ops;
mod dispatch;
mod metatable;
mod boolean;
mod numeric;
mod string;
mod tuple;
mod iterator;
mod misc;

pub use tuple::Tuple;
pub use misc::{Marker, UserData};
pub use numeric::{int_from_str, float_from_str};
pub use iterator::UserIterator;

use misc::Nil;


// TODO replace this

// Type tag for Sphinx's "primitive" types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Boolean,
    Marker,
    Integer,
    Float,
    String,
    Tuple,
    Function,
    Iterator,
    Metatable,
    Object,
    Error,
    UserData,
}

impl Type {
    pub fn name(&self) -> StringValue {
        let name = match self {
            Self::Nil => static_symbol!("nil"),
            Self::Boolean => static_symbol!("bool"),
            Self::Marker => static_symbol!("marker"),
            Self::Integer => static_symbol!("int"),
            Self::Float => static_symbol!("float"),
            Self::String => static_symbol!("string"),
            Self::Tuple => static_symbol!("tuple"),
            Self::Function => static_symbol!("function"),
            Self::Iterator => static_symbol!("iterator"),
            Self::Metatable => static_symbol!("metatable"),
            Self::Object => static_symbol!("object"),
            Self::Error => static_symbol!("error"),
            Self::UserData => static_symbol!("userdata"),
        };
        name.into()
    }
}

impl fmt::Display for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name().with_str(|s| fmt.write_str(s))
    }
}


#[allow(unused_variables)]
pub trait MetaObject {
    fn type_tag(&self) -> Type;
    
    fn type_name(&self) -> ExecResult<StringValue> {
        Ok(self.type_tag().name())
    }
    
    // formatting
    fn fmt_repr(&self) -> ExecResult<StringValue>;
    
    // primitive coercions
    fn as_bool(&self) -> ExecResult<bool> { Ok(true) }
    fn as_bits(&self) -> Option<ExecResult<IntType>> { self.as_int() }
    fn as_int(&self) -> Option<ExecResult<IntType>> { None }
    fn as_float(&self) -> Option<ExecResult<FloatType>> { None }
    
    // iterators
    
    // see iterator.rs for more detail on the iterator API
    fn iter_init(&self) -> Option<ExecResult<IterState>> { None }
    fn iter_next(&self, state: &Variant) -> Option<ExecResult<Variant>> { None }
    fn iter_get(&self, state: &Variant) -> Option<ExecResult<Variant>> { None }
    
    // collections
    fn len(&self) -> Option<ExecResult<usize>> { None }
    //fn getitem(&self, item: &Variant) -> Option<ExecResult<Variant>> { None }
    //fn setitem(&self, item: &Variant) -> Option<ExecResult<Variant>> { None }
    
    // callable
    fn invoke(&self, args: &[Variant]) -> Option<ExecResult<Call>> { None }
    
    // unary operators
    fn op_neg(&self) -> Option<ExecResult<Variant>> { None }
    fn op_pos(&self) -> Option<ExecResult<Variant>> { None }
    fn op_inv(&self) -> Option<ExecResult<Variant>> { None }

    // arithmetic operators
    fn op_mul(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn op_rmul(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn op_div(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn op_rdiv(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn op_mod(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn op_rmod(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn op_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn op_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn op_sub(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn op_rsub(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    // bitwise operators
    fn op_and(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn op_rand(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn op_xor(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn op_rxor(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn op_or(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn op_ror(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn op_shl(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn op_rshl(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn op_shr(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn op_rshr(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    // comparisons
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> { None }
    fn cmp_lt(&self, other: &Variant) -> Option<ExecResult<bool>> { None }
    fn cmp_le(&self, other: &Variant) -> Option<ExecResult<bool>> { None }
}


impl Variant {
    pub fn as_bool(&self) -> ExecResult<bool> { 
        self.as_meta().as_bool()
    }
    
    pub fn as_bits(&self) -> ExecResult<IntType> {
        self.as_meta().as_bits()
            .ok_or_else(|| RuntimeError::metamethod_not_supported(self, MethodTag::AsBits))?
    }
    
    pub fn as_int(&self) -> ExecResult<IntType> {
        self.as_meta().as_int()
            .ok_or_else(|| RuntimeError::metamethod_not_supported(self, MethodTag::AsInt))?
    }
    
    pub fn as_float(&self) -> ExecResult<FloatType> {
        self.as_meta().as_float()
            .ok_or_else(|| RuntimeError::metamethod_not_supported(self, MethodTag::AsFloat))?
    }
}

impl Variant {
    pub fn len(&self) -> ExecResult<usize> {
        self.as_meta().len()
            .ok_or_else(|| RuntimeError::metamethod_not_supported(self, MethodTag::Len))?
    }
    
    pub fn is_empty(&self) -> ExecResult<bool> {
        Ok(self.len()? == 0)
    }
    
    pub fn iter_init(&self) -> ExecResult<IterState> {
        self.as_meta().iter_init()
            .ok_or_else(|| RuntimeError::metamethod_not_supported(self, MethodTag::IterInit))?
    }
    
    pub fn iter_next(&self, state: &Variant) -> ExecResult<Variant> {
        self.as_meta().iter_next(state)
            .ok_or_else(|| RuntimeError::metamethod_not_supported(self, MethodTag::IterNext))?
    }
    
    pub fn iter_get(&self, state: &Variant) -> ExecResult<Variant> {
        self.as_meta().iter_get(state)
            .ok_or_else(|| RuntimeError::metamethod_not_supported(self, MethodTag::IterItem))?
    }
    
    pub fn invoke(&self, args: &[Variant]) -> ExecResult<Call> {
        self.as_meta().invoke(args)
            .ok_or_else(|| RuntimeError::metamethod_not_supported(self, MethodTag::Invoke))?
    }
    
    pub fn fmt_repr(&self) -> ExecResult<StringValue> {
        self.as_meta().fmt_repr()
    }
}

// Set of supported metamethods
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MethodTag {
    Invoke,
    Len,
    IterInit,
    IterNext,
    IterItem,
    AsBool,
    AsBits,
    AsInt,
    AsFloat,
    FmtRepr,
}

impl MethodTag {
    pub fn method_name(&self) -> &'static str {
        match self {
            Self::Invoke => "call",
            
            // iterators and iterables
            Self::IterInit => "iter_init",
            Self::IterNext => "iter_next",
            Self::IterItem => "iter_get",
            
            // sequences
            Self::Len => "len",
            
            // primitive coercion
            Self::AsBool => "bool",
            Self::AsBits => "bits",
            Self::AsInt => "int",
            Self::AsFloat => "float",
            
            // misc
            Self::FmtRepr => "repr",
        }
    }
}

impl fmt::Display for MethodTag {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt.write_str(self.method_name())
    }
}