use core::fmt;
use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::function::{Call, Callable};
use crate::runtime::strings::{StringValue, StringSymbol, static_symbol};
use crate::runtime::errors::{ExecResult, ErrorKind};


mod ops;
mod dispatch;
mod metatable;
mod numeric;
mod string;
mod tuple;
mod native;

pub use tuple::Tuple;
pub use native::UserData;


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
    UserData,
}

impl Type {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Nil => "nil",
            Self::Boolean => "bool",
            Self::Integer => "int",
            Self::Float => "float",
            Self::String => "string",
            Self::Tuple => "tuple",
            Self::Function => "function",
            Self::Metatable => "metatable",
            Self::Object => "object",
            Self::UserData => "userdata",
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(self.name())
    }
}

#[allow(unused_variables)]
pub trait MetaObject {
    fn type_tag(&self) -> Type;
    
    fn type_name(&self) -> ExecResult<StringValue> {
        Ok(StringValue::new_maybe_interned(self.type_tag().name()))
    }
    
    // formatting
    fn fmt_echo(&self) -> ExecResult<StringValue>;
    fn to_string(&self) -> ExecResult<StringValue> { self.fmt_echo() }
    
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
        self.as_meta().as_bool()
    }
    
    pub fn as_bits(&self) -> ExecResult<IntType> {
        self.as_meta().as_bits()
            .ok_or_else(|| ErrorKind::MethodNotSupported(self.type_tag(), MethodTag::AsBits))?
    }
    
    pub fn as_int(&self) -> ExecResult<IntType> {
        self.as_meta().as_int()
            .ok_or_else(|| ErrorKind::MethodNotSupported(self.type_tag(), MethodTag::AsInt))?
    }
    
    pub fn as_float(&self) -> ExecResult<FloatType> {
        self.as_meta().as_float()
            .ok_or_else(|| ErrorKind::MethodNotSupported(self.type_tag(), MethodTag::AsFloat))?
    }
    
    pub fn len(&self) -> ExecResult<usize> {
        self.as_meta().len()
            .ok_or_else(|| ErrorKind::MethodNotSupported(self.type_tag(), MethodTag::Len))?
    }
    
    pub fn is_empty(&self) -> ExecResult<bool> {
        Ok(self.len()? == 0)
    }
    
    pub fn invoke(&self, args: &[Variant]) -> ExecResult<Call> {
        self.as_meta().invoke(args)
            .ok_or_else(|| ErrorKind::MethodNotSupported(self.type_tag(), MethodTag::Invoke))?
    }
    
    pub fn fmt_echo(&self) -> ExecResult<StringValue> {
        self.as_meta().fmt_echo()
    }
    
    pub fn to_string(&self) -> ExecResult<StringValue> {
        self.as_meta().to_string()
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
    
    fn fmt_echo(&self) -> ExecResult<StringValue> {
        Ok(StringValue::from(static_symbol!("nil")))
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
    
    fn fmt_echo(&self) -> ExecResult<StringValue> {
        match self {
            true => Ok(static_symbol!("true").into()),
            false => Ok(static_symbol!("false").into()),
        }
    }
}

impl<F> MetaObject for Gc<F> where F: GcTrace, Gc<F>: Callable {
    fn type_tag(&self) -> Type { Type::Function }
    
    fn invoke(&self, args: &[Variant]) -> Option<ExecResult<Call>> {
        Some(self.checked_call(args))
    }
    
    fn cmp_eq(&self, other: &Variant) -> Option<ExecResult<bool>> {
        match other {
            Variant::Function(other) => Some(Ok(Gc::ptr_eq(self, other))),
            Variant::NativeFunction(other) => Some(Ok(Gc::ptr_eq(self, other))),
            _ => Some(Ok(false)),
        }
    }
    
    fn fmt_echo(&self) -> ExecResult<StringValue> {
        // TODO cache this in the signature struct
        let result = format!(
            "<{} at {:#X}>", self.signature().display_short(), Gc::as_id(self)
        );
        Ok(StringValue::new_uninterned(result))
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
    ToString,
    Echo,
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
            
            Self::ToString => "tostring",
            Self::Echo => "echo",
        }
    }
}

impl fmt::Display for MethodTag {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt.write_str(self.method_name())
    }
}