use std::io;
use std::io::Write;
use std::path::PathBuf;
use clap::{App, Arg};

use string_interner::StringInterner;

use rlo_interpreter::source::{ModuleSource, SourceType, SourceText, ParseContext};

use rlo_interpreter::frontend::render_parser_error;
use rlo_interpreter::debug::symbol::DebugSymbol;
use rlo_interpreter::debug::symbol::DebugSymbolResolver;

use rlo_interpreter::language;
use rlo_interpreter::parser::{Parser, ParserError};
use rlo_interpreter::parser::stmt::{Stmt, StmtVariant};
use rlo_interpreter::interpreter::runtime::{Runtime, Environment};
use rlo_interpreter::interpreter::eval::eval_expr;


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
        let mut interner = StringInterner::default();
        
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
                    .filter_map(|err| err.debug_symbol())
                    .collect::<Vec<DebugSymbol>>();
                    
                let resolved_table = module.resolve_symbols(symbols.iter()).unwrap();
                
                for error in errors.iter() {
                    let resolved = resolved_table.get(&error.debug_symbol().unwrap()).unwrap().as_ref().unwrap();
                    println!("{}", render_parser_error(&error, resolved));
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
    let stmt = match parser.next_stmt().unwrap() {
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

