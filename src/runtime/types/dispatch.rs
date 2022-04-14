use once_cell::sync::Lazy;
use crate::language::{IntType, FloatType};
use crate::runtime::{Variant, GC};
use crate::runtime::function::{Function, NativeFunction};
use crate::runtime::strings::StringSymbol;
use crate::runtime::types::Type;
use crate::runtime::types::metatable::*;
use crate::runtime::errors::{ExecResult, ErrorKind, RuntimeError};


// static METATABLE_NIL: Lazy<Metatable<()>> = Lazy::new(Metatable::default);
// static METATABLE_BOOL: Lazy<Metatable<bool>> = Lazy::new(Metatable::default);
// static METATABLE_INT: Lazy<Metatable<IntType>> = Lazy::new(Metatable::default);
// static METATABLE_FLOAT: Lazy<Metatable<FloatType>> = Lazy::new(Metatable::default);

static METATABLE_STRING: Lazy<Metatable<StringSymbol>> = Lazy::new(Metatable::default);
static METATABLE_TUPLE: Lazy<Metatable<Box<[Variant]>>> = Lazy::new(Metatable::default);
static METATABLE_FUN: Lazy<Metatable<Function>> = Lazy::new(Metatable::default);
static METATABLE_NATIVE_FUN: Lazy<Metatable<NativeFunction>> = Lazy::new(Metatable::default);









/*/// Dispatch layer between Variant and Metamethods
enum MetaDispatch {
    Nil,
    Boolean(bool),
    Integer(IntType),
    Float(FloatType),
    String(StringSymbol),
    Tuple(Box<[Variant]>),
    Function(GC<Function>),
    NativeFunction(GC<NativeFunction>),
}

impl From<Variant> for MetaDispatch {
    fn from(value: Variant) -> Self {
        match value {
            Variant::Nil => Self::Nil,
            Variant::BoolTrue => Self::Boolean(true),
            Variant::BoolFalse => Self::Boolean(true),
            Variant::Integer(value) => Self::Integer(MetaObject::new(value, &METATABLE_INT)),
            Variant::Float(value) => Self::Float(MetaObject::new(value, &METATABLE_FLOAT)),
            Variant::String(value) => Self::String(MetaObject::new(value, &METATABLE_STRING)),
            Variant::EmptyTuple => Type::Tuple,
            Variant::Tuple(..) => Type::Tuple,
            Variant::Function(..) => Type::Function,
            Variant::NativeFunction(..) => Type::Function,
        }
    }
    
}

struct MetaObject<'m, T> {
    value: T,
    metatable: &'m Metatable<T>,
}

impl<'m, T> MetaObject<'m, T> {
    pub fn new(value: T, metatable: &'m Metatable<T>) -> Self {
        Self { value, metatable }
    }
}
    
trait MetaObject {
    fn apply_unary(&self) -> ExecResult<Option<Variant>>;
    fn apply_binary(&self) -> ExecResult<Option<Variant>>;
    fn apply_binary_reflected(&self) -> ExecResult<Option<Variant>>;
}*/