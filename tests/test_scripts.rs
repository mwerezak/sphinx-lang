use std::path::Path;

use sphinx;
use sphinx::builtins;
use sphinx::source::ModuleSource;
use sphinx::codegen::{Program, CompiledProgram};
use sphinx::runtime::{Module, VirtualMachine};
use sphinx::runtime::errors::{ExecResult, ErrorKind};


fn build_program(source: &ModuleSource) -> Option<CompiledProgram> {
    match sphinx::build_module(source) {
        Err(errors) => {
            sphinx::print_build_errors(&errors, source);
            None
        },
        
        Ok(program) => Some(program)
    }
}

fn run_test_script(path: &Path) -> ExecResult<()> {
    let source = ModuleSource::File(path.into());
    let build = build_program(&source).expect("build failed");
    
    let program = Program::load(build.program);
    
    let main_env = builtins::create_prelude();
    let main_module = Module::with_env(Some(source), program.data, main_env);
    
    let vm = VirtualMachine::new(main_module, &program.main);
    vm.run()?;
    
    Ok(())
}

macro_rules! test_script {
    ( $name:tt, $path:expr ) => {
        #[test]
        fn $name() {
            if let Err(error) = run_test_script(Path::new($path)) {
                panic!("{}{}", error.traceback(), error);
            }
        }
    };
    ( $name:tt, $path:expr, error: $error:pat ) => {
        #[test]
        fn $name() {
            let error = run_test_script(Path::new($path)).unwrap_err();
            assert!(matches!(error.kind(), $error));
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
    
    test_script!(loop_, "tests/loop/loop.sph");
    test_script!(continue_, "tests/loop/continue.sph");
}

mod tuple_tests {
    use super::*;
    
    test_script!(assignment, "tests/tuple/assignment.sph");
    test_script!(comparison, "tests/tuple/comparison.sph");
}

mod while_tests {
    use super::*;
    
    test_script!(while_, "tests/while/while.sph");
    test_script!(continue_, "tests/while/continue.sph");
}

mod for_tests {
    use super::*;
    
    test_script!(for_, "tests/for/for.sph");
    test_script!(continue_, "tests/for/continue.sph");
}


mod iterator_tests {
    use super::*;
    
    test_script!(zip_unzip, "tests/iterators/zip_unzip.sph");
}

mod variable_tests {
    use super::*;
    
    test_script!(early_bound, "tests/variable/early_bound.sph");
    test_script!(in_middle_of_block, "tests/variable/in_middle_of_block.sph");
    test_script!(in_nested_block, "tests/variable/in_nested_block.sph");
    test_script!(redeclare_global, "tests/variable/redeclare_global.sph");
    test_script!(assign_to_outer_block, "tests/variable/assign_to_outer_block.sph");
}

mod function_tests {
    use super::*;
    
    test_script!(empty_body, "tests/function/empty_body.sph");
    test_script!(local_recursion, "tests/function/local_recursion.sph");
    test_script!(nested_call_with_arguments, "tests/function/nested_call_with_arguments.sph");
    test_script!(inner_block, "tests/function/inner_block.sph");
    test_script!(missing_arguments, "tests/function/missing_arguments.sph", error: ErrorKind::MissingArguments {..});
    test_script!(argument_unpack, "tests/function/argument_unpack.sph");
}

mod closure_tests {
    use super::*;
    
    test_script!(open_closure_in_function, "tests/closure/open_closure_in_function.sph");
    test_script!(assign_to_upvalue, "tests/closure/assign_to_upvalue.sph");
    test_script!(nested_closure, "tests/closure/nested_closure.sph");
}