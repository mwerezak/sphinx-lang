use std::path::Path;

use sphinx_lang;
use sphinx_lang::stdlib;
use sphinx_lang::source::ModuleSource;
use sphinx_lang::codegen::{Program, CompiledProgram};
use sphinx_lang::runtime::{Module, VirtualMachine, GC};


fn build_program(source: &ModuleSource) -> Option<CompiledProgram> {
    match sphinx_lang::build_module(source) {
        Err(errors) => {
            sphinx_lang::print_build_errors(&errors, source);
            None
        },
        
        Ok(program) => Some(program)
    }
}

fn run_test_script(path: &Path) {
    let source = ModuleSource::File(path.into());
    let build = build_program(&source).expect("build failed");
    
    let program = Program::load(build.program);
    
    let main_env = GC::allocate(stdlib::prelude_env());
    let main_module = Module::with_env(Some(source), program.data, main_env);
    
    let vm = VirtualMachine::new(main_module, &program.main);
    if let Err(error) = vm.run() {
        panic!("{}{}", error.traceback(), error);
    }
}

macro_rules! test_script {
    ( $name:tt, $path:expr ) => {
        #[test]
        fn $name() {
            run_test_script(Path::new($path))
        }
    };
}



test_script!(empty_file, "tests/empty_file.sph");
test_script!(precedence, "tests/precedence.sph");

mod if_tests {
    use super::*;
    
    test_script!(elif, "tests/if/elif.sph");
    test_script!(else_, "tests/if/else.sph");
    test_script!(if_, "tests/if/if.sph");
    test_script!(truth, "tests/if/truth.sph");
}

mod loop_tests {
    use super::*;
    
    // break not yet implemented :(
    // test_script!(loop_, "tests/loop/loop.sph");  
}

mod tuple_tests {
    use super::*;
    
    test_script!(assignment, "tests/tuple/assignment.sph");
}

mod while_tests {
    use super::*;
    
    test_script!(while_, "tests/while/while.sph");
}

mod closure_tests {
    use super::*;
    
    // test_script!(nested_closure, "tests/closure/nested_closure.sph");
}