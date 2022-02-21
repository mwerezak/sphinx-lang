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
    }
}

fn exec_cmd(cmd: &str) {
    let lexer = language::create_default_lexer_rules().build(cmd.chars());
    let mut parser = Parser::new(lexer);
    
    // println!("{:?}", parser.parse_primary());
}