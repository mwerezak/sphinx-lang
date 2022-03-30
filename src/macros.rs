

#[doc(hidden)]
#[macro_export]
macro_rules! __namespace_item {
    ( $namespace:expr, let $name:tt $value:tt ) => {
        $namespace.create(stringify!($name).into(), Access::ReadOnly, Variant::from($value));
    };
    
    ( $namespace:expr, var $name:tt $value:tt ) => {
        $namespace.create(stringify!($name).into(), Access::ReadWrite, Variant::from($value));
    };
    
    ( $namespace:expr, fun _ $func:expr ) => {
        let func = $func;
        let name = func.signature().name().unwrap();
        $namespace.create(name, Access::ReadOnly, Variant::from(func));
    };
    
    ( $namespace:expr, fun $name:tt $func:expr ) => {
        $namespace.create(stringify!($name).into(), Access::ReadOnly, Variant::from($func));
    };
}


/// Helper to create a namespace
#[macro_export]
macro_rules! namespace {
    { $( $item:tt $name:tt = $value:expr ; )* } => {
        let mut namespace = Namespace::new();
        
        $(
            __namespace_item!(namespace, $item $name $value );
        )*
        
        namespace
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
    ( $name:tt ) => { Some(Parameter::new_var(stringify!($name))) };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __defaults {
    () => { None };
    ( $( $default_value:tt )+ ) => { 
        Some(vec![ $( Variant::from($default_value) ),+ ].into_boxed_slice())
    };
}

#[macro_export]
macro_rules! native_function {
    
    // with default params
    ( $func_name:tt $( : $( $required:tt ),+ ; )? $( defaults: $( $default:tt = $default_value:expr ),+ ; )? $( ... $variadic:tt ; )? => $body:expr ) => {
        {
            let signature = Signature::new(
                Some(stringify!($func_name)),
                vec![ $( $( Parameter::new_var(stringify!($required)) ),+ )? ],
                vec![ $( $( Parameter::new_var(stringify!($default)) ),+ )? ],
                __variadic!( $( $variadic )? ),
            );
            
            let defaults = __defaults!( $( $( $default_value )+ )? );
            
            fn body(self_fun: &NativeFunction, args: &[Variant]) -> ExecResult<Variant> {
                const _ARGC: usize = __count!( $( $( $required )+ )? $( $( $default )+ )? );
                
                let mut _argbuf = [Variant::Nil; _ARGC];
                let _bound = self_fun.signature().bind_args(args, self_fun.defaults(), &mut _argbuf);
                let _rest = _bound.args;
                
                $( $( let ($required, _rest) = _rest.split_first().unwrap(); )+ )?
                $( $( let ($default, _rest) = _rest.split_first().unwrap(); )+ )?
                
                $( let $variadic = _bound.varargs; )?
                
                $body
            }
            
            NativeFunction::new(signature, defaults, body)
        }
    };


}