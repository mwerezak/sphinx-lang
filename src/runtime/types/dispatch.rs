///! Enum-based static dispatch for `MetaObject`

use core::ops::Deref;
use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::function::{Call, Function, NativeFunction, Callable};
use crate::runtime::strings::{StringValue, StringSymbol};
use crate::runtime::errors::{ExecResult, ErrorKind};
use crate::runtime::types::{Type, MetaObject, Tuple, UserData};


/// Newtype wrapper for `Variant` that impls `MetaObject` using enum-based static dispatch.
struct MetaDispatch<'a>(&'a Variant);

impl Variant {
    #[inline]
    pub fn as_meta(&self) -> impl MetaObject + '_ {
        MetaDispatch(self)
    }
    
    pub fn type_tag(&self) -> Type {
        self.as_meta().type_tag()
    }
    
    pub fn type_name(&self) -> ExecResult<StringValue> {
        self.as_meta().type_name()
    }
}

macro_rules! static_dispatch {
    { fn $name:tt ( $( $arg:tt : $argty:ty ),* ) -> $return:ty } => {
        fn $name (&self, $( $arg : $argty ),* ) -> $return {
            match self.0 {
                Variant::Nil => <() as MetaObject>::$name(&(), $( $arg ),* ),
                Variant::BoolTrue => <bool as MetaObject>::$name(&true, $( $arg ),* ),
                Variant::BoolFalse => <bool as MetaObject>::$name(&false, $( $arg ),* ),
                Variant::Integer(value) => <IntType as MetaObject>::$name(value, $( $arg ),* ),
                Variant::Float(value) => <FloatType as MetaObject>::$name(value, $( $arg ),* ),
                
                Variant::InternStr(symbol) => <StringValue as MetaObject>::$name(&(*symbol).into(), $( $arg ),* ),
                Variant::InlineStr(inline) => <StringValue as MetaObject>::$name(&(*inline).into(), $( $arg ),* ),
                Variant::GCStr(gc_str) => <StringValue as MetaObject>::$name(&(*gc_str).into(), $( $arg ),* ),
                
                Variant::Tuple(tuple) => <Tuple as MetaObject>::$name(tuple, $( $arg ),* ),
                
                Variant::Function(fun) => <Gc<Function> as MetaObject>::$name(fun, $( $arg ),* ),
                Variant::NativeFunction(fun) => <Gc<NativeFunction> as MetaObject>::$name(fun, $( $arg ),* ),
                
                Variant::UserData(data) => <(dyn UserData + 'static) as MetaObject>::$name(&**data, $( $arg ),* ),
            }
        }
    };
}

impl MetaObject for MetaDispatch<'_> {
    
    static_dispatch!{ fn type_tag() -> Type }
    
    static_dispatch!{ fn type_name() -> ExecResult<StringValue> }
    
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
