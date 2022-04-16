use core::fmt;
use core::convert::AsRef;
use once_cell::sync::Lazy;

use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::function::{Call, Function, NativeFunction, Callable};
use crate::runtime::strings::{StringValue, StringSymbol};
use crate::runtime::errors::{ExecResult, ErrorKind};

mod ops;
mod dispatch;
mod metatable;
mod numeric;
mod string;
mod tuple;

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
    
    // iterators
    fn next(&self) -> Option<ExecResult<Variant>> { None }
    
    // data collections
    fn len(&self) -> Option<ExecResult<usize>> { None }
    fn iter(&self) -> Option<ExecResult<Variant>> { None }
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
    
    fn op_inv(&self) -> Option<ExecResult<Variant>> { 
        Some(Ok(Variant::from(!(*self)))) 
    }
    
    fn op_and(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(false))),
            Variant::BoolTrue => Some(Ok(Variant::from(*self))),
            _ => None,
        }
    }
    fn op_rand(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_and(lhs)
    }
    
    fn op_xor(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(*self))),
            Variant::BoolTrue => Some(Ok(Variant::from(!(*self)))),
            _ => None,
        }
    }
    fn op_rxor(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_and(lhs)
    }
    
    fn op_or(&self, rhs: &Variant) -> Option<ExecResult<Variant>> {
        match rhs {
            Variant::BoolFalse => Some(Ok(Variant::from(*self))),
            Variant::BoolTrue => Some(Ok(Variant::from(true))),
            _ => None,
        }
    }
    fn op_ror(&self, lhs: &Variant) -> Option<ExecResult<Variant>> {
        self.op_or(lhs)
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
    Next,
    Iter,
    AsBool,
    AsBits,
    AsInt,
    AsFloat,
}

impl MethodTag {
    pub fn method_name(&self) -> &'static str {
        match self {
            Self::Invoke => "call",
            
            // iterators and iterables
            Self::Next => "next",
            Self::Iter => "iter",
            
            // sequences
            Self::Len => "len",
            
            // primitive coercion
            Self::AsBool => "bool",
            Self::AsBits => "bits",
            Self::AsInt => "int",
            Self::AsFloat => "float",
        }
    }
}

impl fmt::Display for MethodTag {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt.write_str(self.method_name())
    }
}