use crate::runtime::Runtime;
use crate::runtime::data::Variant;
use crate::runtime::types::RuntimeType;
use crate::runtime::types::builder::RuntimeTypeBuilder;
use crate::runtime::errors::{RuntimeResult, RuntimeError, ErrorKind};

// Integers

fn int_add(lhs: Variant, rhs: Variant) -> RuntimeResult<Variant> {
    unimplemented!()
}

fn create_int_type(runtime: &mut Runtime) -> &mut RuntimeType {
    RuntimeTypeBuilder::new("int", "int")
        .set_add(int_add)
        .build(runtime).unwrap()
}