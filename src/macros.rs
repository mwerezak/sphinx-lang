

#[doc(hidden)]
#[macro_export]
macro_rules! __namespace_item {
    ( $namespace:expr, let $name:tt $value:tt ) => {
        $namespace.create(
            stringify!($name).into(), 
            crate::runtime::module::Access::ReadOnly, 
            crate::runtime::Variant::from($value)
        );
    };
    
    ( $namespace:expr, var $name:tt $value:tt ) => {
        $namespace.create(
            stringify!($name).into(), 
            crate::runtime::module::Access::ReadWrite, 
            crate::runtime::Variant::from($value)
        );
    };
    
    ( $namespace:expr, fun _ $func:expr ) => {
        let func = $func;
        let name = func.signature().name().unwrap();
        $namespace.create(
            name, 
            crate::runtime::module::Access::ReadOnly, 
            crate::runtime::Variant::from(func)
        );
    };
    
    ( $namespace:expr, fun $name:tt $func:expr ) => {
        $namespace.create(
            stringify!($name).into(), 
            crate::runtime::module::Access::ReadOnly, 
            crate::runtime::Variant::from($func)
        );
    };
}


/// Helper to create a namespace
#[macro_export]
macro_rules! namespace {
    ( $namespace:expr, { $( $item:tt $name:tt = $value:expr ; )* } ) => {
        $(
            __namespace_item!($namespace, $item $name $value );
        )*
    };
}


/// Helper macros for creating native functions
#[doc(hidden)]
#[macro_export]
macro_rules! __count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + __count!($($xs)*));
}

#[doc(hidden)]
#[macro_export]
macro_rules! __variadic {
    () => { None };
    ( $name:tt ) => { Some(crate::runtime::function::Parameter::new_var(stringify!($name))) };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __defaults {
    () => { None };
    ( $( $default_value:tt )+ ) => { 
        Some(vec![ $( crate::runtime::Variant::from($default_value) ),+ ].into_boxed_slice())
    };
}

#[macro_export]
macro_rules! native_function {
    
    // with default params
    ( $func_name:tt, $env:expr $( , this ( $self_name:tt ) )? $( , vm ( $vm_name:tt ) )? $( , params ( $( $required:tt ),+ ) )? $( , defaults ( $( $default:tt = $default_value:expr ),+ ) )? $( , variadic ( $variadic:tt ) )? => $body:expr ) => {
        {
            type Variant = crate::runtime::Variant;
            type Signature = crate::runtime::function::Signature;
            type Parameter = crate::runtime::function::Parameter;
            type NativeFunction = crate::runtime::function::NativeFunction;
            type VirtualMachine<'a> = crate::runtime::vm::VirtualMachine<'a>;
            type ExecResult<T> = crate::runtime::errors::ExecResult<T>;
            
            let signature = Signature::new(
                Some(stringify!($func_name)),
                vec![ $( $( Parameter::new_var(stringify!($required)) ),+ )? ],
                vec![ $( $( Parameter::new_var(stringify!($default)) ),+ )? ],
                __variadic!( $( $variadic )? ),
            );
            
            let defaults = __defaults!( $( $( $default_value )+ )? );
            
            fn body(self_fun: &NativeFunction, _vm: &mut VirtualMachine<'_>, args: &[Variant]) -> ExecResult<Variant> {
                const _ARGC: usize = __count!( $( $( $required )+ )? $( $( $default )+ )? );
                
                let mut _argbuf = [Variant::Nil; _ARGC];
                let _bound = self_fun.signature().bind_args(args, self_fun.defaults(), &mut _argbuf);
                let _rest = _bound.args;
                
                $( let $self_name = self_fun; )?
                $( let $vm_name = _vm; )?
                $( $( let ($required, _rest) = _rest.split_first().unwrap(); )+ )?
                $( $( let ($default, _rest) = _rest.split_first().unwrap(); )+ )?
                $( let $variadic = _bound.varargs; )?
                
                $body
            }
            
            NativeFunction::new(signature, defaults, $env, body)
        }
    };


}