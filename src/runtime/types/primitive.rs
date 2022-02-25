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

fn int_neg(operand: Variant) -> RuntimeResult<Variant> {
    Ok(Variant::Integer(-operand.int_value().unwrap()))
}

fn int_pos(operand: Variant) -> RuntimeResult<Variant> {
    Ok(operand)
}

fn int_mul(lhs: Variant, rhs: Variant) -> RuntimeResult<Option<Variant>> {
    let lhs_value = lhs.int_value().unwrap();
    
    match rhs {
        Variant::Integer(rhs_value) => multiply_int(lhs_value, rhs_value),
        Variant::Float(rhs_value) => multiply_float(lhs_value as language::FloatType, rhs_value),
        _ => return Ok(None),
    }
}

fn int_div(lhs: Variant, rhs: Variant) -> RuntimeResult<Option<Variant>> {
    let lhs_value = lhs.int_value().unwrap();
    
    match rhs {
        Variant::Integer(rhs_value) => divide_int(lhs_value, rhs_value),
        Variant::Float(rhs_value) => divide_float(lhs_value as language::FloatType, rhs_value),
        _ => return Ok(None),
    }
}
fn int_rdiv(rhs: Variant, lhs: Variant) -> RuntimeResult<Option<Variant>> {
    int_div(lhs, rhs)
}

fn int_add(lhs: Variant, rhs: Variant) -> RuntimeResult<Option<Variant>> {
    let lhs_value = lhs.int_value().unwrap();
    
    let result = match rhs {
        Variant::Integer(value) => Variant::Integer(lhs_value + value),
        Variant::Float(value) => Variant::Float(lhs_value as language::FloatType + value),
        _ => return Ok(None),
    };
    Ok(Some(result))
}

pub fn create_int_type(runtime: &mut Runtime) -> RuntimeResult<&mut RuntimeType> {
    RuntimeTypeBuilder::new("int", "int")
        .set_neg(int_neg)
        .set_add(int_add)
        .build(runtime)
}


// Numeric Operations

fn multiply_int(lhs: language::IntType, rhs: language::IntType) -> RuntimeResult<Option<Variant>> {
    Ok(Some(Variant::Integer(lhs * rhs)))
}
fn multiply_float(lhs: language::FloatType, rhs: language::FloatType) -> RuntimeResult<Option<Variant>> {
    Ok(Some(Variant::Float(lhs * rhs)))
}

fn divide_int(lhs: language::IntType, rhs: language::IntType) -> RuntimeResult<Option<Variant>> {
    if rhs == 0 {
        Err(RuntimeError::new(ErrorKind::DivideByZero))
    } else {
        Ok(Some(Variant::Integer(lhs * rhs)))
    }
}
fn divide_float(lhs: language::FloatType, rhs: language::FloatType) -> RuntimeResult<Option<Variant>> {
    if rhs == 0.0 {
        Err(RuntimeError::new(ErrorKind::DivideByZero))
    } else {
        Ok(Some(Variant::Float(lhs * rhs)))
    }
}

fn add_int(lhs: language::IntType, rhs: language::IntType) -> RuntimeResult<Option<Variant>> {
    Ok(Some(Variant::Integer(lhs + rhs)))
}
fn add_float(lhs: language::FloatType, rhs: language::FloatType) -> RuntimeResult<Option<Variant>> {
    Ok(Some(Variant::Float(lhs + rhs)))
}