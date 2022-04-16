
use crate::runtime::{Variant, GC};
use crate::runtime::strings::StringSymbol;
use crate::runtime::module::{Namespace, GlobalEnv, Access};
use crate::runtime::function::{NativeFunction, Signature, Parameter};
use crate::runtime::errors::ExecResult;


// examples for testing

use std::time::SystemTime;
use crate::runtime::ops;

pub fn create_prelude() -> GC<GlobalEnv> {
    let env = GlobalEnv::new();
    
    let time = native_function!(time, env, _  => {
        let time = SystemTime::UNIX_EPOCH
            .elapsed()
            .unwrap()
            .as_secs_f64();
        Ok(Variant::from(time))
    });
    
    // example using env
    let radians = native_function!(radians, env, this, params(degrees) => {
        let result = degrees.as_float()?/180.0 * this.env().borrow().lookup(&StringSymbol::intern("PI"))?.as_float()?;
        Ok(Variant::from(result))
    });
    
    // Contrived example to show handling of default values and variadics is supported
    let add_example = native_function!(add_example, env, _,
        params(a),
        defaults(b = 1),
        variadic(varargs) => {
            
        // for value in varargs.iter() {
        //     println!("{}", value);
        // }
        println!("{:?}", varargs);
        a.apply_add(b)
    });
    
    namespace!(env.borrow_mut(), {
        let PI = core::f64::consts::PI;
        fun _ = time;
        fun _ = radians;
        fun _ = add_example;
    });
    
    // let env = 
    
    // namespace
    env
}