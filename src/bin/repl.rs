use std::io;
use std::io::Write;
use std::path::PathBuf;

use log;

use clap::{Command, Arg};


use sphinx_lang::source::{ModuleSource, SourceType, ParseContext};
use sphinx_lang::frontend::render_parser_error;
use sphinx_lang::debug::symbol::DebugSymbolResolver;
use sphinx_lang::language;
use sphinx_lang::interpreter::{EvalContext, ExecContext};
use sphinx_lang::lexer::LexerBuilder;
use sphinx_lang::parser::stmt::{Stmt};
use sphinx_lang::runtime::*;
use sphinx_lang::runtime::strings::StringTableGuard;


fn main() {
    env_logger::init();
    
    let app = Command::new("repl")
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
        let string_table = StringTableGuard::new();
        
        let mut parse_ctx = ParseContext::new(&lexer_factory, &string_table);
        let source_text = module.source_text().expect("error reading source");
        let parse_result = parse_ctx.parse_ast(source_text);
        
        match parse_result {
            Ok(stmts) => {
                println!("{:#?}", stmts);
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
        println!("\nSphinx Interpreter {}\n", version);
        let string_table = StringTableGuard::new();
        let repl = Repl::new(&string_table);
        repl.run();
    }
}


const PROMT_START: &str = ">>> ";
const PROMT_CONTINUE: &str = "... ";

struct Repl<'r> {
    string_table: &'r StringTableGuard,
    lexer_factory: LexerBuilder,
    root_env: Environment<'r, 'r>,
}


enum ReadLine {
    Ok(String),
    Empty,
    Restart,
    Quit,
}

impl<'r> Repl<'r> {
    pub fn new(string_table: &'r StringTableGuard) -> Self {
        Repl { 
            string_table,
            lexer_factory: language::create_default_lexer_rules(),
            root_env: new_root_env(string_table),
        }
    }
    
    fn read_line(&self, prompt: &'static str) -> ReadLine {
        io::stdout().write(prompt.as_bytes()).unwrap();
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
        
        if input.chars().last().unwrap() == '\x04' || input == "quit" {
            return ReadLine::Quit;
        }
        
        ReadLine::Ok(input)
    }
    
    fn is_input_complete(&self, input: &str) -> bool {
        // This is fairly hacky. If we can't parse the input without errors, then we assume we need to continue
        let mut parse_ctx = ParseContext::new(&self.lexer_factory, &self.string_table);
        let module = ModuleSource::new("<repl>", SourceType::String(input.to_string()));
        let source_text = module.source_text().expect("error reading source");
        let parse_result = parse_ctx.parse_ast(source_text);
        parse_result.is_ok()
    }
    
    pub fn run(&self) {
        
        loop {
            let mut input = String::new();
            
            loop {
                let prompt =
                    if input.is_empty() { PROMT_START }
                    else { PROMT_CONTINUE };
                
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
                        
                        if self.is_input_complete(input.as_str()) {
                            break
                        }
                        
                        input.push('\n')
                    }
                }
            }
            
            let mut parse_ctx = ParseContext::new(&self.lexer_factory, &self.string_table);
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
            
            for stmt in stmts.iter() {
                match stmt.variant() {
                    Stmt::Expression(expr) => {
                        let eval_ctx = EvalContext::new(&self.root_env);
                        let eval_result = eval_ctx.eval_expr(&expr);
                        log::debug!("{:?}", eval_result);
                        
                        match eval_result {
                            Ok(value) => {
                                let mut buf = String::new();
                                value.unwrap_value().write_repr(&mut buf, &self.string_table)
                                    .expect("could not write to string buffer");
                                
                                println!("{}", buf);
                            },
                            Err(error) => {
                                println!("{:?}", error)
                            },
                        }
                    },
                    _ => {
                        let exec_ctx = ExecContext::new(&self.root_env);
                        let exec_result = exec_ctx.exec(&stmt);
                        log::debug!("{:?}", exec_result);
                    },
                }
            }
            
        }
        
    }
}
