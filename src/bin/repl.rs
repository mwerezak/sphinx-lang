use std::path::PathBuf;
use clap::{App, Arg};

use string_interner::StringInterner;

use rlo_interpreter::source::{ModuleSource, SourceType, SourceText};

use rlo_interpreter::language;
use rlo_interpreter::lexer::LexerBuilder;
use rlo_interpreter::parser::Parser;

// use rlo_interpreter::runtime::{Runtime, placeholder_module};

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
    
    let mut source = None;
    
    if let Some(s) = args.value_of("cmd") {
        source = Some(SourceType::String(s.to_string()));
    } else if let Some(s) = args.value_of("file") {
        source = Some(SourceType::File(PathBuf::from(s)))
    }
    
    if let Some(source) = source {
        let lexer_factory = language::create_default_lexer_rules();
        
        let module = ModuleSource::new("main", source);
        let mut interner = StringInterner::default();
        match module.source_text().expect("error reading source") {
            SourceText::String(s) => {
                let mut chars = Vec::new();
                chars.extend(s.chars().map(|c| Ok(c)));
                
                let lexer = lexer_factory.build_once(chars.into_iter());
                let mut parser = Parser::new(&module, &mut interner, lexer);
                match parser.next_expr() {
                    Err(error) => println!("{}", error),
                    Ok(expr) => println!("{:#?}", expr),
                };
                
            }
            SourceText::File(readf) => {
                let lexer = lexer_factory.build_once(readf);
                let mut parser = Parser::new(&module, &mut interner, lexer);
                match parser.next_expr() {
                    Err(error) => println!("{}", error),
                    Ok(expr) => println!("{:#?}", expr),
                };
            },
        };
        
        
    } else {
        // println!("\nReLox Interpreter {}\n", version);
        // let mut repl = Repl::new(">>> ");
        // repl.run();
    }
}

// fn exec_cmd(cmd: &str) {
//     // let module = placeholder_module("<cmd>");
//     // let mut runtime = language::create_runtime();
//     // let mut parser = runtime.create_parser(&module, cmd.chars());
    
//     // print_eval_str(&mut runtime, &module, cmd);
// }

// fn exec_file(path: &Path) {
//     // let mut runtime = language::create_runtime();
    
//     let source = fs::read_to_string(path).unwrap();
//     let filename = format!("{}", path.display());
//     // let module = placeholder_module(filename.as_str());
    
//     // print_eval_str(&mut runtime, &module, source.as_str());
// }


// struct Repl {
//     prompt: &'static str,
//     // runtime: Runtime,
// }

// impl Repl {
//     pub fn new(prompt: &'static str) -> Self {
//         Repl { 
//             prompt, 
//             // runtime: language::create_runtime(),
//         }
//     }
    
//     pub fn run(&mut self) {
        
//         // let module = placeholder_module("<repl>");
        
//         loop {
//             io::stdout().write(self.prompt.as_bytes()).unwrap();
//             io::stdout().flush().unwrap();
            
//             let mut input = String::new();
//             let result = io::stdin().read_line(&mut input);
//             if result.is_err() {
//                 println!("Could not read input: {}", result.unwrap_err());
//                 continue;
//             }
            
//             while let Some('\n' | '\r') = input.chars().next_back() {
//                 input.pop();
//             }
            
//             if input.is_empty() {
//                 continue;
//             }
            
//             if input.chars().last().unwrap() == '\x04' || input == "quit" {
//                 break;
//             }
            
//             // print_eval_str(&mut self.runtime, &module, input.as_str());
//         }
        
//     }
// }

// use rlo_interpreter::runtime::{Module, placeholder_runtime_ctx};
// use rlo_interpreter::parser::expr::{ExprMeta};
// use rlo_interpreter::parser::ParserError;
// fn print_result(result: Result<ExprMeta, ParserError>) {
//     match result {
//         Ok(expr) => println!("{:?}", expr),
//         Err(error) => println!("{}", error),
//     }
// }

// fn print_eval_str(runtime: &mut Runtime, module: &Module, input: &str) {
//     let mut parser = runtime.create_parser(module, input.chars());
//     let expr = match parser.next_expr() {
//         Err(error) => return println!("{}", error),
//         Ok(expr) => expr,
//     };

//     let mut local_ctx = placeholder_runtime_ctx(runtime);
//     let eval_result = local_ctx.eval(expr.expr());
//     println!("{:?}", eval_result);
// }