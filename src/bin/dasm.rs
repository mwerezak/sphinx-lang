use std::path::PathBuf;
use clap::{Command, Arg};

use sphinx_lang::frontend;
use sphinx_lang::language;
use sphinx_lang::source::{ModuleSource, SourceType, ParseContext};
use sphinx_lang::parser::ParserError;
use sphinx_lang::vm::codegen::CodeGenerator;
use sphinx_lang::debug::symbol::DebugSymbolResolver;
use sphinx_lang::debug::dasm::Disassembler;

fn print_parse_errors<'m>(module: &'m ModuleSource, errors: Vec<ParserError<'m>>) {
    let symbols = errors.iter().map(|err| err.debug_symbol());
        
    let resolved_table = module.resolve_symbols(symbols).unwrap();
    
    for error in errors.iter() {
        let resolved = resolved_table.get(&error.debug_symbol()).unwrap().as_ref();
        
        match resolved {
            Ok(resolved) => println!("{}", frontend::render_parser_error(&error, resolved)),
            Err(resolve_error) => {
                println!("{}", error);
                println!("Could not resolve symbol: {}", resolve_error);
            }
        }
    }
}

fn main() {
    env_logger::init();
    
    let app = Command::new("repl")
        .version("0.0")
        .author("M. Werezak <mwerezak@gmail.com>")
        .about("Bytecode disassembler for the Sphinx programming language")
        .arg(
            Arg::new("file")
            .index(1)
            .help("path to input file")
            .value_name("FILE")
        )
        .arg(
            Arg::new("source")
            .short('s')
            .help("disassemble a snippet then exit")
            .value_name("CMD")
        )
        .arg(
            Arg::new("cmd")
            .short('c')
            .help("disassemble a snippet then exit")
            .value_name("CMD")
        );
        
    let version = app.get_version().unwrap();
    let args = app.get_matches();
    
    let mut module = None;
    if let Some(s) = args.value_of("cmd") {
        let source = SourceType::String(s.to_string());
        module = Some(ModuleSource::new("<cmd>", source));
    } else if let Some(s) = args.value_of("source") {
        let source = SourceType::File(PathBuf::from(s));
        module = Some(ModuleSource::new(s, source));
    } else if let Some(_s) = args.value_of("file") {
        // TODO read compiled bytecode and go directly to disassembly
        unimplemented!()
    }
    
    if module.is_none() {
        println!("No input.");
        return;
    }
    
    println!("\nSphinx Version {}\n", version);
    
    // compile source
    let module = module.unwrap();
    
    // parsing
    let lexer_factory = language::create_default_lexer_rules();
    let mut parse_ctx = ParseContext::new(&lexer_factory);
    let source_text = module.source_text().expect("error reading source");
    let parse_result = parse_ctx.parse_ast(source_text);
    
    if parse_result.is_err() {
        println!("Errors in file \"{}\":\n", module.name());
        print_parse_errors(&module, parse_result.unwrap_err());
        return;
    }
    
    let stmts = parse_result.unwrap();
    let symbols = stmts.iter().map(|stmts| stmts.debug_symbol());
    let symbol_table = module.resolve_symbols(symbols);
    
    // compilation
    let codegen = CodeGenerator::new();
    let compile_result = codegen.compile_program(stmts.iter());
    if compile_result.is_err() {
        unimplemented!();
    }
    
    let program = compile_result.unwrap();
    let dasm = {
        let dasm = Disassembler::new(&program.bytecode)
            .with_symbols(&program.symbols);
        
        if let Ok(ref symbol_table) = symbol_table {
            dasm.with_symbol_table(&symbol_table)
        } else {
            dasm
        }
    };
    
    println!("== \"{}\" ==", module.name());
    println!("{}", dasm);
}