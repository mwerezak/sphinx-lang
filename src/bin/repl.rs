use std::io;
use std::io::Write;
use std::path::PathBuf;
use clap::{App, Arg};

use string_interner::StringInterner;

use rlo_interpreter::source::{ModuleSource, SourceType, SourceText};

use rlo_interpreter::frontend::render_parser_error;
use rlo_interpreter::debug::symbol::DebugSymbolResolver;

use rlo_interpreter::language;
use rlo_interpreter::parser::Parser;
use rlo_interpreter::parser::stmt::StmtVariant;
use rlo_interpreter::interpreter::runtime::{Runtime, Environment};
use rlo_interpreter::interpreter::eval::eval_expr;


fn main() {
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
        
        let mut interner = StringInterner::default();
        match module.source_text().expect("error reading source") {
            SourceText::String(s) => {
                let mut chars = Vec::new();
                chars.extend(s.chars().map(Ok));
                
                let lexer = lexer_factory.build_once(chars.into_iter());
                let mut parser = Parser::new(&module, &mut interner, lexer);
                match parser.placeholder_toplevel() {
                    Err(error) => println!("{}", error),
                    Ok(stmt) => println!("{:#?}", stmt),
                };
                
            }
            SourceText::File(readf) => {
                let lexer = lexer_factory.build_once(readf);
                let mut parser = Parser::new(&module, &mut interner, lexer);
                match parser.placeholder_toplevel() {
                    Ok(stmt) => println!("{:#?}", stmt),
                    Err(error) => {
                        let symbol = error.debug_symbol().unwrap();
                        let resolved = module.resolve_symbols(std::iter::once(&symbol)).unwrap();
                        let symbol = resolved.get(&symbol).unwrap().as_ref().unwrap();
                        
                        println!("{}", render_parser_error(&error, symbol));
                    },
                };
            },
        };
        
        
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
        
        // let module = placeholder_module("<repl>");
        
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
            
            print_eval_str(&mut self.runtime, input.as_str());
        }
        
    }
}

// use rlo_interpreter::runtime::{Module, placeholder_runtime_ctx};
// use rlo_interpreter::parser::expr::{ExprMeta};
// use rlo_interpreter::parser::ParserError;
// fn print_result(result: Result<ExprMeta, ParserError>) {
//     match result {
//         Ok(expr) => println!("{:?}", expr),
//         Err(error) => println!("{}", error),
//     }
// }

fn print_eval_str(runtime: &mut Runtime, input: &str) {
    let string = input.to_string();
    let module = ModuleSource::new("<main>", SourceType::String(string.clone()));
    
    let mut chars = Vec::new();
    chars.extend(string.chars().map(Ok));
    
    let lexer = runtime.lexer_factory.build(chars.into_iter());
    let mut parser = Parser::new(&module, &mut runtime.interner, lexer);
    let stmt = match parser.placeholder_toplevel() {
        Ok(expr) => expr,
        Err(error) => return println!("{} [{:?}]", error, error.debug_symbol()),
    };

    let scope = Environment { runtime };
    
    if let StmtVariant::Expression(expr) = stmt.variant() {
        let eval_result = eval_expr(&scope, &expr, Some(stmt.debug_symbol()));
        println!("{:?}", eval_result);
    } else {
        println!("{:?}", stmt);
    }
    
    

    // let mut local_ctx = placeholder_runtime_ctx(runtime);
    // let eval_result = local_ctx.eval(expr.expr());
    // println!("{:?}", eval_result);
}

