use crate::runtime::Gc;
use crate::runtime::module::NamespaceEnv;
use crate::runtime::errors::RuntimeError;


pub fn create_misc_builtins(env: Gc<NamespaceEnv>) {
    
    let repr = native_function!(repr, env, params(value) => {
        Ok(Variant::from(value.fmt_repr()?))
    });
    
    let print = native_function!(print, env, variadic(values) => {
        if let Some((first, rest)) = values.split_first() {
            print!("{}", first.fmt_str()?);
            for value in rest.iter() {
                print!(" ");
                print!("{}", value.fmt_str()?);
            }
        }
        println!();
        
        Ok(Variant::Nil)
    });
    
    // Produces a tuple of the global names in the current call frame
    // TODO return an object or a namespace instead?
    let globals = native_function!(globals, env, vm(vm)  => {
        let global_env = vm.frame().module().globals();
        let names = global_env.borrow().names()
            .map(|name| Variant::from(*name))
            .collect::<Vec<Variant>>()
            .into_boxed_slice();
            
        Ok(Variant::from(names))
    });
    
    // Prints the signature of a function. Will print an object's docstring if that is ever added.
    let help = native_function!(help, env, params(object) => {
        let signature = match object {
            Variant::Function(fun) => fun.signature().fmt_signature(),
            Variant::NativeFunction(fun) => fun.signature().fmt_signature(),
            _ => return Err(RuntimeError::invalid_value("not a function"))
        };
        
        println!("{}", signature);
        Ok(Variant::Nil)
    });
    
    namespace_insert!(env.borrow_mut(), {
        fun _ = globals;
        fun _ = repr;
        fun _ = print;
        fun _ = help;
    });
}
