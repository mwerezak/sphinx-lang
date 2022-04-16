use core::fmt;
use core::ops::Deref;
use core::convert::AsRef;
use once_cell::sync::Lazy;

use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::function::{Call, Function, NativeFunction, Callable};
use crate::runtime::strings::{StringValue, StringSymbol};
use crate::runtime::errors::{ExecResult, ErrorKind};

pub mod operator;
pub mod metatable;
pub mod numeric;
pub mod string;
pub mod tuple;

pub use tuple::Tuple;


// Type tag for Sphinx's "primitive" types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Nil,
    Boolean,
    Integer,
    Float,
    String,
    Tuple,
    Function,
    Metatable,
    Object,
}


impl fmt::Display for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Nil => "nil",
            Self::Boolean => "bool",
            Self::Integer => "int",
            Self::Float => "float",
            Self::String => "string",
            Self::Tuple => "tuple",
            Self::Function => "function",
            Self::Metatable => "metatable",
            
            // note, when looking up the type of an object value 
            // the object's metatable should be used to generate the type name
            Self::Object => "object",
        };
        fmt.write_str(name)
    }
}

#[allow(unused_variables)]
pub trait MetaObject {
    fn type_tag(&self) -> Type;
    
    // primitive coercions
    fn as_bool(&self) -> ExecResult<bool> { Ok(true) }
    fn as_bits(&self) -> Option<ExecResult<IntType>> { self.as_int() }
    fn as_int(&self) -> Option<ExecResult<IntType>> { None }
    fn as_float(&self) -> Option<ExecResult<FloatType>> { None }
    
    // data
    fn len(&self) -> Option<ExecResult<usize>> { None }
    //fn get_item(&self, item: &Variant) -> Option<ExecResult<Variant>> { None }
    
    // callable
    fn invoke(&self, args: &[Variant]) -> Option<ExecResult<Call>> { None }
    
    // unary operators
    fn apply_neg(&self) -> Option<ExecResult<Variant>> { None }
    fn apply_pos(&self) -> Option<ExecResult<Variant>> { None }
    fn apply_inv(&self) -> Option<ExecResult<Variant>> { None }

    // arithmetic operators
    fn apply_mul(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rmul(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_div(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rdiv(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_mod(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rmod(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_add(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_radd(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_sub(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rsub(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    // bitwise operators
    fn apply_and(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rand(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_xor(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rxor(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_or(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_ror(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_shl(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rshl(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    fn apply_shr(&self, rhs: &Variant) -> Option<ExecResult<Variant>> { None }
    fn apply_rshr(&self, lhs: &Variant) -> Option<ExecResult<Variant>> { None }
    
    // comparisons
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> { None }
    fn cmp_lt(&self, other: &Variant) -> Option<ExecResult<bool>> { None }
    fn cmp_le(&self, other: &Variant) -> Option<ExecResult<bool>> { None }
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

impl Deref for MetaDispatch {
    type Target = dyn MetaObject;
    
    #[inline]
    fn deref(&self) -> &Self::Target {
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

impl Variant {
    #[inline]
    pub fn as_meta(&self) -> impl Deref<Target=dyn MetaObject> {
        match self {
            Self::Nil => MetaDispatch::Nil,
            Self::BoolTrue => MetaDispatch::Boolean(true),
            Self::BoolFalse => MetaDispatch::Boolean(false),
            Self::Integer(value) => MetaDispatch::Integer(*value),
            Self::Float(value) => MetaDispatch::Float(*value),
            
            Self::InternStr(symbol) => MetaDispatch::String((*symbol).into()),
            Self::InlineStr(inline) => MetaDispatch::String((*inline).into()),
            Self::GCStr(gc_str) => MetaDispatch::String((*gc_str).into()),
            
            Self::Tuple(tuple) => MetaDispatch::Tuple(*tuple),
            Self::Function(fun) => MetaDispatch::Function(*fun),
            Self::NativeFunction(fun) => MetaDispatch::NativeFunction(*fun),
        }
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
    
    pub fn as_bool(&self) -> ExecResult<bool> { 
        match self {
            Self::Nil => Ok(false),
            Self::BoolFalse => Ok(false),
            Self::BoolTrue => Ok(true),
            _ => self.as_meta().as_bool(),
        }
    }
    
    pub fn as_bits(&self) -> ExecResult<IntType> {
        match self {
            Self::BoolFalse => false.as_bits().unwrap(),
            Self::BoolTrue => true.as_bits().unwrap(),
            Self::Integer(value) => Ok(*value),
            _ => self.as_meta().as_bits()
                .ok_or_else(|| ErrorKind::MethodNotSupported(self.type_tag(), MethodTag::AsBits))?,
        }
    }
    
    pub fn as_int(&self) -> ExecResult<IntType> {
        match self {
            Self::Integer(value) => Ok(*value),
            _ => self.as_meta().as_int()
                .ok_or_else(|| ErrorKind::MethodNotSupported(self.type_tag(), MethodTag::AsInt))?
        }
    }
    
    pub fn as_float(&self) -> ExecResult<FloatType> {
        match self {
            Self::Float(value) => Ok(*value),
            Self::Integer(value) => Ok(*value as FloatType),
            _ => self.as_meta().as_float()
                .ok_or_else(|| ErrorKind::MethodNotSupported(self.type_tag(), MethodTag::AsFloat))?
        }
    }
    
    pub fn len(&self) -> ExecResult<usize> {
        match self {
            Self::Tuple(tuple) => <Tuple as MetaObject>::len(tuple).unwrap(),
            _ => self.as_meta().len()
                .ok_or_else(|| ErrorKind::MethodNotSupported(self.type_tag(), MethodTag::Len))?
        }
    }
    
    pub fn invoke(&self, args: &[Variant]) -> ExecResult<Call> {
        match self {
            Self::Function(fun) => fun.checked_call(args),
            Self::NativeFunction(fun) => fun.checked_call(args),
            _ => self.as_meta().invoke(args)
                .ok_or_else(|| ErrorKind::MethodNotSupported(self.type_tag(), MethodTag::Invoke))?
        }
    }
}


// Nil
impl MetaObject for () {
    fn type_tag(&self) -> Type { Type::Nil }
    
    fn as_bool(&self) -> ExecResult<bool> { Ok(false) }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Nil => Some(Ok(true)),
            _ => None,
        }
    }
}

// Booleans
impl MetaObject for bool {
    fn type_tag(&self) -> Type { Type::Boolean }
    
    fn as_bool(&self) -> ExecResult<bool> { Ok(*self) }
    
    fn as_bits(&self) -> Option<ExecResult<IntType>> {
        if *self { Some(Ok(!0)) } // all 1s
        else { Some(Ok(0)) } // all 0s
    }
    
    fn apply_inv(&self) -> Option<ExecResult<Variant>> { 
        Some(Ok(Variant::from(!(*self)))) 
    }
    
    fn apply_and(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(false))),
            Variant::BoolTrue => Some(Ok(Variant::from(*self))),
            _ => None,
        }
    }
    fn apply_rand(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_and(lhs)
    }
    
    fn apply_xor(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(*self))),
            Variant::BoolTrue => Some(Ok(Variant::from(!(*self)))),
            _ => None,
        }
    }
    fn apply_rxor(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_and(lhs)
    }
    
    fn apply_or(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(*self))),
            Variant::BoolTrue => Some(Ok(Variant::from(true))),
            _ => None,
        }
    }
    fn apply_ror(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.apply_or(lhs)
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::BoolFalse => Some(Ok(!(*self))),
            Variant::BoolTrue => Some(Ok(*self)),
            _ => None,
        }
    }
}

impl<F> MetaObject for GC<F> where F: GCTrace, GC<F>: Callable {
    fn type_tag(&self) -> Type { Type::Function }
    
    fn invoke(&self, args: &[Variant]) -> Option<ExecResult<Call>> {
        Some(self.checked_call(args))
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        other.as_gc().map(|other| Ok(GC::ptr_eq(&(*self).into(), &other)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MethodTag {
    Invoke,
    Len,
    AsBool,
    AsBits,
    AsInt,
    AsFloat,
}

impl MethodTag {
    pub fn method_name(&self) -> &'static str {
        match self {
            Self::Invoke => "__call",
            Self::Len => "__len",
            Self::AsBool => "__bool",
            Self::AsBits => "__bits",
            Self::AsInt => "__int",
            Self::AsFloat => "__float",
        }
    }
}

impl fmt::Display for MethodTag {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt.write_str(self.method_name())
    }
}