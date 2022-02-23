use std::fs;
use std::io;
use std::io::Write;
use std::path::Path;
use clap::{App, Arg};

use string_interner::StringInterner;
use rlo_interpreter::language;
use rlo_interpreter::parser::Parser;

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
        let repl = Repl::new(">>> ");
        repl.run();
    }
}

fn exec_cmd(cmd: &str) {
    let lexer = language::create_default_lexer_rules().build(cmd.chars());
    let mut interner = StringInterner::new();
    let mut parser = Parser::new("<cmd>", &mut interner, lexer);
    
    println!("{:?}", parser.next_expr());
}

fn exec_file(path: &Path) {
    let source = fs::read_to_string(path).unwrap();
    let filename = format!("{}", path.display());
    
    let lexer = language::create_default_lexer_rules().build(source.chars());
    let mut interner = StringInterner::new();
    let mut parser = Parser::new(filename.as_str(), &mut interner, lexer);
    
    println!("{:?}", parser.next_expr());
}


struct Repl {
    prompt: &'static str,
}

impl Repl {
    pub fn new(prompt: &'static str) -> Self {
        Repl { prompt }
    }
    
    pub fn run(&self) {
        let mut interner = StringInterner::new();
        
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
            
            if input.chars().next().unwrap() == '\x04' || input == "quit" {
                break;
            }
            
            let factory = language::create_default_lexer_rules();
            let lexer = factory.build(input.chars());
            let mut parser = Parser::new("<repl>", &mut interner, lexer);
            println!("{:?}", parser.next_expr());
        }
    }
}
