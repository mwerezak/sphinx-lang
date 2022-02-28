use crate::language::{IntType, FloatType};
use crate::runtime::variant::{Variant, Primitive};
use crate::runtime::errors::{EvalResult, EvalErrorKind};

// Unary Operators

pub fn eval_neg(operand: &Variant) -> EvalResult<Variant> {
    let value = match operand {
        Variant::Integer(value) => Variant::Integer(-value),
        Variant::Float(value) => Variant::Float(-value),
        _ => unimplemented!(),
    };
    Ok(value)
}

pub fn eval_pos(operand: &Variant) -> EvalResult<Variant> {
    if operand.pri_type().is_numeric() {
        return Ok(*operand);
    }
    
    unimplemented!()
}

pub fn eval_inv(operand: &Variant) -> EvalResult<Variant> {
    if let Variant::Integer(value) = operand {
        return Ok(Variant::Integer(!value))
    }
    
    unimplemented!()
}

pub fn eval_not(operand: &Variant) -> EvalResult<Variant> {
    return Ok(Variant::Boolean(!operand.truth_value()))
}


// Binary Operators

// Arithmetic Operations

pub fn eval_mul(lhs: &Variant, rhs: &Variant) -> EvalResult<Variant> {
    let value = match (lhs.pri_type(), rhs.pri_type()) {
        (Primitive::Integer, Primitive::Integer) => int_mul(lhs.int_value()?, rhs.int_value()?),
        (lt, rt) if lt.is_numeric() && rt.is_numeric() => float_mul(lhs.float_value()?, rhs.float_value()?),
        _ => unimplemented!(),
    };
    Ok(value)
}

fn int_mul(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs * rhs) }
fn float_mul(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs * rhs) }


pub fn eval_div(lhs: &Variant, rhs: &Variant) -> EvalResult<Variant> {
    let value = match (lhs.pri_type(), rhs.pri_type()) {
        (Primitive::Integer, Primitive::Integer) => int_div(lhs.int_value()?, rhs.int_value()?),
        (lt, rt) if lt.is_numeric() && rt.is_numeric() => float_mul(lhs.float_value()?, rhs.float_value()?),
        _ => unimplemented!(),
    };
    Ok(value)
}

fn int_div(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs / rhs) }
fn float_div(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs / rhs) }


pub fn eval_mod(lhs: &Variant, rhs: &Variant) -> EvalResult<Variant> {
    let value = match (lhs.pri_type(), rhs.pri_type()) {
        (Primitive::Integer, Primitive::Integer) => int_mod(lhs.int_value()?, rhs.int_value()?),
        (lt, rt) if lt.is_numeric() && rt.is_numeric() => float_mul(lhs.float_value()?, rhs.float_value()?),
        _ => unimplemented!(),
    };
    Ok(value)
}

fn int_mod(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs % rhs) }
fn float_mod(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs % rhs) }


pub fn eval_add(lhs: &Variant, rhs: &Variant) -> EvalResult<Variant> {
    let value = match (lhs.pri_type(), rhs.pri_type()) {
        (Primitive::Integer, Primitive::Integer) => int_add(lhs.int_value()?, rhs.int_value()?),
        (lt, rt) if lt.is_numeric() && rt.is_numeric() => float_mul(lhs.float_value()?, rhs.float_value()?),
        _ => unimplemented!(),
    };
    Ok(value)
}

fn int_add(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs + rhs) }
fn float_add(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs + rhs) }


pub fn eval_sub(lhs: &Variant, rhs: &Variant) -> EvalResult<Variant> {
    let value = match (lhs.pri_type(), rhs.pri_type()) {
        (Primitive::Integer, Primitive::Integer) => int_sub(lhs.int_value()?, rhs.int_value()?),
        (lt, rt) if lt.is_numeric() && rt.is_numeric() => float_mul(lhs.float_value()?, rhs.float_value()?),
        _ => unimplemented!(),
    };
    Ok(value)
}

fn int_sub(lhs: IntType, rhs: IntType) -> Variant { Variant::Integer(lhs - rhs) }
fn float_sub(lhs: FloatType, rhs: FloatType) -> Variant { Variant::Float(lhs - rhs) }