use std::fs;
use std::io;
use std::io::Write;
use std::path::Path;
use clap::{App, Arg};

use rlo_interpreter::language;
use rlo_interpreter::runtime::Runtime;

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
    
    if let Some(s) = args.value_of("cmd") {
        exec_cmd(s);
    } else if let Some(s) = args.value_of("file") {
        let file_path = Path::new(s);
        exec_file(&file_path);
    } else {
        println!("\nReLox Interpreter {}\n", version);
        let mut repl = Repl::new(">>> ");
        repl.run();
    }
}


use rlo_interpreter::parser::expr::ExprMeta;
use rlo_interpreter::parser::ParserError;
fn print_result(result: Result<ExprMeta, ParserError>) {
    match result {
        Ok(expr) => println!("{:?}", expr),
        Err(error) => println!("{}", error),
    }
}

fn exec_cmd(cmd: &str) {
    let mut runtime = Runtime::new(language::create_default_lexer_rules());
    let mut parser = runtime.create_parser("<cmd>", cmd.chars());
    print_result(parser.next_expr());
}

fn exec_file(path: &Path) {
    let mut runtime = Runtime::new(language::create_default_lexer_rules());
    
    let source = fs::read_to_string(path).unwrap();
    let filename = format!("{}", path.display());
    let mut parser = runtime.create_parser(filename.as_str(), source.chars());
    print_result(parser.next_expr());
}


struct Repl {
    prompt: &'static str,
    runtime: Runtime,
}

impl Repl {
    pub fn new(prompt: &'static str) -> Self {
        Repl { 
            prompt, 
            runtime: Runtime::new(language::create_default_lexer_rules()),
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
            
            let mut parser = self.runtime.create_parser("<repl>", input.chars());
            print_result(parser.next_expr());
        }
        
    }
}
