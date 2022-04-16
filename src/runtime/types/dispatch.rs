///! Enum-based static dispatch for `MetaObject`

use core::ops::Deref;
use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::function::{Call, Function, NativeFunction, Callable};
use crate::runtime::strings::{StringValue, StringSymbol};
use crate::runtime::errors::{ExecResult, ErrorKind};
use crate::runtime::types::{Type, MetaObject, Tuple};


impl Variant {
    
    #[inline]
    pub fn as_meta(&self) -> impl MetaObject {
        MetaDispatch::from(*self)
    }
    
    pub fn type_tag(&self) -> Type {
        match self {
            Self::Nil => ().type_tag(),
            Self::BoolTrue => true.type_tag(),
            Self::BoolFalse => false.type_tag(),
            Self::Integer(value) => value.type_tag(),
            Self::Float(value) => value.type_tag(),
            
            Self::InternStr(symbol) => StringValue::from(*symbol).type_tag(),
            Self::InlineStr(inline) => StringValue::from(*inline).type_tag(),
            Self::GCStr(gc_str) => StringValue::from(*gc_str).type_tag(),
            
            Self::Tuple(tuple) => tuple.type_tag(),
            Self::Function(fun) => fun.type_tag(),
            Self::NativeFunction(fun) => fun.type_tag(),
        }
    }
}


// allows Variant to store types that don't impl MetaObject, 
// but can be converted into one (e.g. string types)
enum MetaDispatch {
    Nil,
    Boolean(bool),
    Integer(IntType),
    Float(FloatType),
    String(StringValue),
    Tuple(Tuple),
    Function(GC<Function>),
    NativeFunction(GC<NativeFunction>),
}

impl From<Variant> for MetaDispatch {
    fn from(value: Variant) -> Self {
        match value {
            Variant::Nil => Self::Nil,
            Variant::BoolTrue => Self::Boolean(true),
            Variant::BoolFalse => Self::Boolean(false),
            Variant::Integer(value) => Self::Integer(value),
            Variant::Float(value) => Self::Float(value),
            Variant::InternStr(symbol) => Self::String(symbol.into()),
            Variant::InlineStr(inline) => Self::String(inline.into()),
            Variant::GCStr(gc_str) => Self::String(gc_str.into()),
            Variant::Tuple(tuple) => Self::Tuple(tuple),
            Variant::Function(fun) => Self::Function(fun),
            Variant::NativeFunction(fun) => Self::NativeFunction(fun),
        }
    }
}

impl MetaDispatch {
    fn as_dyn(&self) -> &dyn MetaObject {
        match self {
            Self::Nil => &(),
            Self::Boolean(value) => value,
            Self::Integer(value) => value,
            Self::Float(value) => value,
            Self::String(value) => value,
            Self::Tuple(value) => value,
            Self::Function(fun) => &*fun,
            Self::NativeFunction(fun) => &*fun,
        }
    }
}

macro_rules! static_dispatch {
    { fn $name:tt ( $( $arg:tt : $argty:ty ),* ) -> $return:ty } => {
        fn $name (&self, $( $arg : $argty ),* ) -> $return {
            match self {
                Self::Nil => <() as MetaObject>::$name(&(), $( $arg ),* ),
                Self::Boolean(value) => <bool as MetaObject>::$name(value, $( $arg ),* ),
                Self::Integer(value) => <IntType as MetaObject>::$name(value, $( $arg ),* ),
                Self::Float(value) => <FloatType as MetaObject>::$name(value, $( $arg ),* ),
                Self::String(value) => <StringValue as MetaObject>::$name(value, $( $arg ),* ),
                Self::Tuple(value) => <Tuple as MetaObject>::$name(value, $( $arg ),* ),
                Self::Function(fun) => <GC<Function> as MetaObject>::$name(fun, $( $arg ),* ),
                Self::NativeFunction(fun) => <GC<NativeFunction> as MetaObject>::$name(fun, $( $arg ),* ),
            }
        }
    };
}

impl MetaObject for MetaDispatch {
    
    static_dispatch!{ fn type_tag() -> Type }
    
    static_dispatch!{ fn as_bool() -> ExecResult<bool> }
    static_dispatch!{ fn as_bits() -> Option<ExecResult<IntType>> }
    static_dispatch!{ fn as_int() -> Option<ExecResult<IntType>> }
    static_dispatch!{ fn as_float() -> Option<ExecResult<FloatType>> }
    
    // iterators
    static_dispatch!{ fn next() -> Option<ExecResult<Variant>> }
    
    // data collections
    static_dispatch!{ fn len() -> Option<ExecResult<usize>> }
    static_dispatch!{ fn iter() -> Option<ExecResult<Variant>> }
    
    // callable
    static_dispatch!{ fn invoke(args: &[Variant]) -> Option<ExecResult<Call>> }
    
    // unary operators
    static_dispatch!{ fn op_neg() -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_pos() -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_inv() -> Option<ExecResult<Variant>> }

    // arithmetic operators
    static_dispatch!{ fn op_mul(rhs: &Variant) -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_rmul(lhs: &Variant) -> Option<ExecResult<Variant>> }
    
    static_dispatch!{ fn op_div(rhs: &Variant) -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_rdiv(lhs: &Variant) -> Option<ExecResult<Variant>> }
    
    static_dispatch!{ fn op_mod(rhs: &Variant) -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_rmod(lhs: &Variant) -> Option<ExecResult<Variant>> }
    
    static_dispatch!{ fn op_add(rhs: &Variant) -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_radd(lhs: &Variant) -> Option<ExecResult<Variant>> }
    
    static_dispatch!{ fn op_sub(rhs: &Variant) -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_rsub(lhs: &Variant) -> Option<ExecResult<Variant>> }
    
    // bitwise operators
    static_dispatch!{ fn op_and(rhs: &Variant) -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_rand(lhs: &Variant) -> Option<ExecResult<Variant>> }
    
    static_dispatch!{ fn op_xor(rhs: &Variant) -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_rxor(lhs: &Variant) -> Option<ExecResult<Variant>> }
    
    static_dispatch!{ fn op_or(rhs: &Variant) -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_ror(lhs: &Variant) -> Option<ExecResult<Variant>> }
    
    static_dispatch!{ fn op_shl(rhs: &Variant) -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_rshl(lhs: &Variant) -> Option<ExecResult<Variant>> }
    
    static_dispatch!{ fn op_shr(rhs: &Variant) -> Option<ExecResult<Variant>> }
    static_dispatch!{ fn op_rshr(lhs: &Variant) -> Option<ExecResult<Variant>> }
    
    // comparisons
    
    static_dispatch!{ fn cmp_eq(other: &Variant) -> Option<ExecResult<bool>> }
    static_dispatch!{ fn cmp_lt(other: &Variant) -> Option<ExecResult<bool>> }
    static_dispatch!{ fn cmp_le(other: &Variant) -> Option<ExecResult<bool>> }
    
    
}
