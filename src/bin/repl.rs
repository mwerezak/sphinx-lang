use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use clap::{App, Arg};
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
            .required_unless_present("cmd")
        )
        .arg(
            Arg::new("cmd")
            .short('c')
            .help("execute a snippet then exit")
            .value_name("CMD")
            .required_unless_present("file")
        );
        
    let args = app.get_matches();
    
    if let Some(s) = args.value_of("cmd") {
        exec_cmd(s);
    } else if let Some(s) = args.value_of("file") {
        let file_path = Path::new(s);
        exec_file(&file_path)
    }
}

fn exec_cmd(cmd: &str) {
    let lexer = language::create_default_lexer_rules().build(cmd.chars());
    let mut parser = Parser::new("<main>", lexer);
    
    println!("{:?}", parser.next_expr());
}

fn exec_file(path: &Path) {
    let mut source = String::new();
    
    let mut file = File::open(path).unwrap();
    file.read_to_string(&mut source).unwrap();
    
    let filename = format!("{}", path.display());
    let lexer = language::create_default_lexer_rules().build(source.chars());
    let mut parser = Parser::new(filename.as_str(), lexer);
    
    println!("{:?}", parser.next_expr());
}