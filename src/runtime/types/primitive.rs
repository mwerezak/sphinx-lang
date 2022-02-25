use crate::language;
use crate::runtime::Runtime;
use crate::runtime::data::Variant;
use crate::runtime::types::RuntimeType;
use crate::runtime::types::builder::RuntimeTypeBuilder;
use crate::runtime::errors::{RuntimeResult, RuntimeError, ErrorKind};



#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Primitive {
    Nil,
    Boolean,
    Integer,
    Float,
    String,
    Tuple,
    Object,
}

// Integers

fn int_add(lhs: Variant, rhs: Variant) -> RuntimeResult<Option<Variant>> {
    let lhs_value = lhs.int_value().unwrap();
    
    let result = match rhs {
        Variant::Integer(value) => Variant::Integer(lhs_value + value),
        Variant::Float(value) => Variant::Float(lhs_value as language::FloatType + value),
        _ => return Ok(None),
    };
    Ok(Some(result))
}

fn int_neg(operand: Variant) -> RuntimeResult<Variant> {
    Ok(Variant::Integer(-operand.int_value().unwrap()))
}

pub fn create_int_type(runtime: &mut Runtime) -> RuntimeResult<&mut RuntimeType> {
    RuntimeTypeBuilder::new("int", "int")
        .set_neg(int_neg)
        .set_add(int_add)
        .build(runtime)
}