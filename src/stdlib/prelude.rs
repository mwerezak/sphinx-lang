
use crate::language::FloatType;
use crate::runtime::Variant;
use crate::runtime::module::{Namespace, Access};
use crate::runtime::types::function::{Invoke, NativeFunction, Signature, Parameter};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};

crate::use_function_def_helpers!();

// examples for testing

use crate::runtime::ops;

fn add_example() -> NativeFunction {
    native_function!(add_example: a; b = 1; ...args => {
        println!("{:?}", args);
        ops::eval_add(a, b)
    })
}


use std::time::{SystemTime, Duration};

fn time() -> FloatType {
    SystemTime::UNIX_EPOCH
        .elapsed()
        .unwrap()
        .as_secs_f64()
}


pub fn create_prelude() -> Namespace {
    let mut prelude = Namespace::new();
    prelude.create("add_example".into(), Access::ReadOnly, Variant::from(add_example()));
    prelude
}