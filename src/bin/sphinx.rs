use std::io::{self, Write};
use std::path::PathBuf;
use clap::{Command, Arg};

use sphinx::frontend;
use sphinx::source::{ModuleSource, SourceText};
use sphinx::parser::stmt::{StmtMeta, Stmt, StmtList, ControlFlow};
use sphinx::parser::expr::Expr;
use sphinx::parser::primary::Atom;
use sphinx::parser::lvalue::{LValue, AssignType, Assignment};
use sphinx::codegen::{Program, CompiledProgram};
use sphinx::runtime::{Module, VirtualMachine, Gc};
use sphinx::runtime::module::GlobalEnv;
use sphinx::runtime::strings::StringInterner;
use sphinx::debug::symbol::resolver::BufferedResolver;
use sphinx::stdlib;

fn main() {
    env_logger::init();
    
    let app = Command::new("sphinx")
        .version("0.0")
        .author("M. Werezak <mwerezak@gmail.com>")
        .about("An interpreter for the Sphinx programming language")
        .arg(
            Arg::new("file")
            .index(1)
            .help("Path to input script file")
            .value_name("FILE")
        )
        .arg(
            Arg::new("cmd")
            .short('c')
            .help("Execute a snippet then exit")
            .value_name("CMD")
        )
        .arg(
            Arg::new("interactive")
            .short('i')
            .help("Drop into an interactive REPL after executing")
        )
        .arg(
            Arg::new("compile_only")
            .short('d')
            .help("Produce compiled bytecode instead of executing (not implemented)")
        )
        .arg(
            Arg::new("debug")
            .long("debug")
            .help("Enable step-through debugging")
        );
    
    let version = app.get_version().unwrap();
    let args = app.get_matches();
    
    let source;
    if let Some(s) = args.value_of("cmd") {
        source = ModuleSource::String(s.to_string());
    } else if let Some(s) = args.value_of("file") {
        source = ModuleSource::File(PathBuf::from(s));
    } else {
        let repl_env = stdlib::create_prelude();
        Repl::new(version.to_string(), repl_env).run();
        
        return;
    }
    
    if args.is_present("compile_only") {
        unimplemented!()
    }
    else if args.is_present("interactive") {
        if let Some(build) = build_program(&source) {
            let program = Program::load(build.program);
            
            let repl_env = stdlib::create_prelude();
            let main_module = Module::with_env(Some(source), program.data, repl_env);
            
            let vm = VirtualMachine::new(main_module, &program.main);
            if args.is_present("debug") {
                run_debugger(vm);
            } else if let Err(error) = vm.run() {
                println!("{}{}", error.traceback(), error);
            }
            
            Repl::new(version.to_string(), repl_env).run()
        }
    }
    else if let Some(build) = build_program(&source) {
        let program = Program::load(build.program);
        
        let main_env = stdlib::create_prelude();
        let main_module = Module::with_env(Some(source), program.data, main_env);
        
        let vm = VirtualMachine::new(main_module, &program.main);
        if args.is_present("debug") {
            run_debugger(vm);
        } else if let Err(error) = vm.run() {
            println!("{}{}", error.traceback(), error);
        }
    }
}


fn build_program(source: &ModuleSource) -> Option<CompiledProgram> {
    match sphinx::build_module(source) {
        Err(errors) => {
            sphinx::print_build_errors(&errors, source);
            None
        },
        
        Ok(program) => Some(program)
    }
}

fn run_debugger(vm: VirtualMachine) {
    for status in vm.run_steps() {
        match status {
            Err(error) => {
                println!("Runtime error: {:?}", error);
                break;
            }
            Ok(snapshot) => {
                println!("{}", snapshot);
            }
        }
        
        let mut input = String::new();
        let result = io::stdin().read_line(&mut input);
        if let Err(..) = result { }
    }
    
    println!("Execution stopped.");
}

//////// REPL ////////
const PROMT_START: &str = ">>> ";
const PROMT_CONTINUE: &str = "... ";

pub struct Repl {
    version: String,
    repl_env: Gc<GlobalEnv>,
}

enum ReadLine {
    Ok(String),
    Empty,
    Restart,
    Quit,
}

impl Repl {
    pub fn new(version: String, repl_env: Gc<GlobalEnv>) -> Self {
        Self {
            version, repl_env,
        }
    }
    
    fn read_line(&self, prompt: &'static str) -> ReadLine {
        io::stdout().write_all(prompt.as_bytes()).unwrap();
        io::stdout().flush().unwrap();
        
        let mut input = String::new();
        let result = io::stdin().read_line(&mut input);
        if result.is_err() {
            println!("Could not read input: {}", result.unwrap_err());
            return ReadLine::Restart;
        }
        
        input = input.trim_end().to_string();
        
        if input.is_empty() {
            return ReadLine::Empty;
        }
        
        if input == "quit" || input.chars().any(|c| c == '\x04') {
            return ReadLine::Quit;
        }
        
        ReadLine::Ok(input)
    }
    
    pub fn run(&mut self) {
        println!("\nSphinx Version {}\n", self.version);
        
        loop {
            let mut interner;
            let mut input = String::new();
            let mut parse_result = None;
            
            loop {
                let prompt =
                    if input.is_empty() { PROMT_START }
                    else { PROMT_CONTINUE };
                
                interner = StringInterner::new();
                
                match self.read_line(prompt) {
                    ReadLine::Quit => return,
                    ReadLine::Restart => continue,
                    ReadLine::Empty => {
                        if input.is_empty() { continue }
                        else { break }
                    },
                    ReadLine::Ok(line) => {
                        input.push_str(&line);
                        
                        if line.trim_end().ends_with(';') {
                            break
                        }
                        
                        // If we can't parse the input without errors, then we assume we need to continue
                        let source_text = SourceText::from(input.clone());
                        if let Ok(ast) = sphinx::parse_source(&mut interner, source_text) {
                            parse_result.replace(ast);
                            break
                        }
                        
                        input.push('\n')
                    }
                }
            }
            
            let parse_result =
                if let Some(ast) = parse_result { Ok(ast) }
                else { 
                    let source_text = SourceText::from(input.clone());
                    sphinx::parse_source(&mut interner, source_text) 
                };
            
            let mut ast = match parse_result {
                Ok(ast) => ast,
                
                Err(errors) => {
                    let resolver = BufferedResolver::new(input);
                    frontend::print_source_errors(&resolver, &errors);
                    continue;
                },
            };
            
            Self::repl_ast_transform(&mut interner, &mut ast);
            
            let build = match sphinx::compile_ast(interner, ast) {
                Ok(build) => build,
                
                Err(errors) => {
                    let resolver = BufferedResolver::new(input);
                    frontend::print_source_errors(&resolver, &errors);
                    continue;
                }
            };
            
            let program = Program::load(build.program);
            
            let module = Module::with_env(None, program.data, self.repl_env);
            
            let vm = VirtualMachine::new(module, &program.main);
            match vm.run() {
                Ok(value) => if !value.is_nil() {
                    println!("{}", value.display_echo())
                }
                
                Err(error) => println!("{}{}", error.traceback(), error),
            }
            
        }
        
    }
    
    // dirty hack to make the REPL work
    fn repl_ast_transform(interner: &mut StringInterner, ast: &mut Vec<StmtMeta>) {
        let last_stmt = match ast.pop() {
            Some(stmt) => stmt,
            None => return,
        };

        let (stmt, symbol) = last_stmt.take();
        
        let result_expr;
        if let Stmt::Expression(expr) = stmt {
            result_expr = expr;
        } else {
            ast.push(StmtMeta::new(stmt, symbol));
            result_expr = Expr::Atom(Atom::Nil);
        }
        
        // bind the result expression to a global name
        let result_name = interner.get_or_intern("_");
        let result_decl = Expr::Assignment(Box::new(Assignment {
            assign: AssignType::DeclImmutable,
            lhs: LValue::Identifier(result_name),
            rhs: result_expr,
            op: None,
        }));
        ast.push(StmtMeta::new(Stmt::Expression(result_decl), symbol));
        
        let return_result = ControlFlow::Return {
            symbol: None, 
            expr: Some(Box::new(
                Expr::Atom(Atom::Identifier(result_name))
            )),
        };
        
        let wrapper = Stmt::Loop {
            label: None,
            body: StmtList::new(Vec::new(), Some(return_result)),
        };
        ast.push(StmtMeta::new(wrapper, symbol));
        
    }
}

