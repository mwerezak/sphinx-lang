use clap::{App, Arg};

use rlo_interpreter::language;
use rlo_interpreter::lexer::{TokenMeta, Token};

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
        let mut lexer = language::create_default_lexer_rules().build(s.chars());
        
        loop {
            let out = lexer.next_token();
            println!("{:?}", out);
            
            if let Ok(TokenMeta { token: Token::EOF, .. }) = out {
                break;
            }
        }
    }
}