
use crate::language::FloatType;
use crate::runtime::Variant;
use crate::runtime::module::{Namespace, Access};
use crate::runtime::function::{Invoke, NativeFunction, Signature, Parameter};
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};
use crate::stdlib;

// crate::use_function_def_helpers!();

// examples for testing



use std::time::{SystemTime, Duration};
use crate::runtime::ops;

pub(super) fn create_prelude() -> Namespace {
    namespace! {
        let PI = std::f64::consts::PI;
        
        fun _ = native_function!(time => {
            let time = SystemTime::UNIX_EPOCH
                .elapsed()
                .unwrap()
                .as_secs_f64();
            Ok(Variant::from(time))
        });
        
        // Contrived example to show handling of default values and variadics is supported
        fun _ = native_function!(add_example: a; defaults: b = 1; ...varargs; => {
            // for value in varargs.iter() {
            //     println!("{}", value);
            // }
            println!("{:?}", varargs);
            ops::eval_add(a, b)
        });
    }
}