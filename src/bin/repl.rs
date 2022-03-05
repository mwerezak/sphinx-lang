use std::io;
use std::io::Write;
use std::path::PathBuf;

use log;

use clap::{App, Arg};


use rlo_interpreter::source::{ModuleSource, SourceType, ParseContext};
use rlo_interpreter::frontend::render_parser_error;
use rlo_interpreter::debug::symbol::DebugSymbolResolver;
use rlo_interpreter::language;
use rlo_interpreter::interpreter;
use rlo_interpreter::parser::stmt::{StmtVariant};
use rlo_interpreter::runtime::Runtime;
use rlo_interpreter::runtime::data::StringInterner;


fn main() {
    env_logger::init();
    
    let app = App::new("repl")
        .version("0.0")
        .author("M. Werezak <mwerezak@gmail.com>")
        .about("Dynamic language interpreter")
        .arg(
            Arg::new("file")
            .index(1)
            .help("path to input script file")
            .value_name("FILE")
        )
        .arg(
            Arg::new("cmd")
            .short('c')
            .help("execute a snippet then exit")
            .value_name("CMD")
        );
        
    let version = app.get_version().unwrap();
    let args = app.get_matches();
    
    let mut module = None;
    
    if let Some(s) = args.value_of("cmd") {
        let source = SourceType::String(s.to_string());
        module = Some(ModuleSource::new("<cmd>", source));
    } else if let Some(s) = args.value_of("file") {
        let source = SourceType::File(PathBuf::from(s));
        module = Some(ModuleSource::new(s, source));
    }
    
    if let Some(module) = module {
        let lexer_factory = language::create_default_lexer_rules();
        let mut interner = StringInterner::new();
        
        let mut parse_ctx = ParseContext::new(&lexer_factory, &mut interner);
        let source_text = module.source_text().expect("error reading source");
        let parse_result = parse_ctx.parse_ast(source_text);
        
        match parse_result {
            Ok(_stmts) => {
                // TODO
            },
            Err(errors) => { 
                println!("Errors in file \"{}\":\n", module.name());
            
                let symbols = errors.iter()
                    .map(|err| err.debug_symbol());
                    
                let resolved_table = module.resolve_symbols(symbols).unwrap();
                
                for error in errors.iter() {
                    let resolved = resolved_table.get(&error.debug_symbol()).unwrap().as_ref();
                    
                    match resolved {
                        Ok(resolved) => println!("{}", render_parser_error(&error, resolved)),
                        Err(resolve_error) => {
                            println!("{}", error);
                            println!("Could not resolve symbol: {}", resolve_error);
                        }
                    }
                }
            },
        }
        
    } else {
        println!("\nReLox Interpreter {}\n", version);
        let mut repl = Repl::new(">>> ");
        repl.run();
    }
}


struct Repl {
    prompt: &'static str,
    runtime: Runtime,
}

impl Repl {
    pub fn new(prompt: &'static str) -> Self {
        Repl { 
            prompt,
            runtime: Runtime::new(),
        }
    }
    
    pub fn run(&mut self) {
        
        loop {
            io::stdout().write(self.prompt.as_bytes()).unwrap();
            io::stdout().flush().unwrap();
            
            let mut input = String::new();
            let result = io::stdin().read_line(&mut input);
            if result.is_err() {
                println!("Could not read input: {}", result.unwrap_err());
                continue;
            }
            
            while let Some('\n' | '\r') = input.chars().next_back() {
                input.pop();
            }
            
            if input.is_empty() {
                continue;
            }
            
            if input.chars().last().unwrap() == '\x04' || input == "quit" {
                break;
            }
            
            let mut parse_ctx = ParseContext::new(&self.runtime.lexer_factory, &mut self.runtime.string_table);
            let module = ModuleSource::new("<repl>", SourceType::String(input));
            let source_text = module.source_text().expect("error reading source");
            let parse_result = parse_ctx.parse_ast(source_text);
            
            let stmts = match parse_result {
                Ok(stmts) => stmts,
                
                Err(errors) => { 
                    let symbols = errors.iter()
                        .map(|err| err.debug_symbol());
                        
                    let resolved_table = module.resolve_symbols(symbols).unwrap();
                    
                    for error in errors.iter() {
                        let resolved = resolved_table.get(&error.debug_symbol()).unwrap().as_ref();
                        println!("{}", render_parser_error(&error, resolved.unwrap()));
                    }
                    
                    continue;
                },
            };
            
            let mut env = self.runtime.placeholder_env();
            for stmt in stmts.iter() {
                match stmt.variant() {
                    StmtVariant::Expression(expr) => {
                        let eval_result = interpreter::eval(&mut env, &expr);
                        log::debug!("{:?}", eval_result);
                        
                        match eval_result {
                            Ok(value) => {
                                let mut buf = String::new();
                                value.write_repr(&mut buf, env.runtime())
                                    .expect("could not write to string buffer");
                                
                                println!("{}", buf);
                            },
                            Err(_error) => { /* TODO */ },
                        }
                    },
                    stmt => {
                        let exec_result = interpreter::exec(&mut env, &stmt);
                        log::debug!("{:?}", exec_result);
                    },
                }
            }
            
        }
        
    }
}
