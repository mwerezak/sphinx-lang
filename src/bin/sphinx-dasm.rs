use std::path::PathBuf;
use clap::{Command, Arg, ArgMatches};

use sphinx::frontend;
use sphinx::{BuildErrors, build_module};
use sphinx::source::ModuleSource;
use sphinx::runtime::strings::StringInterner;
use sphinx::debug::symbol::DebugSymbolResolver;
use sphinx::debug::dasm::Disassembler;

fn main() {
    env_logger::init();
    
    let app = Command::new("sph-dasm")
        .version("0.0")
        .author("M. Werezak <mwerezak@gmail.com>")
        .about("Bytecode disassembler for the Sphinx programming language")
        .arg(
            Arg::new("source")
            .index(1)
            .help("path to a source file to disassemble")
            .value_name("FILE")
        )
        .arg(
            Arg::new("parse_only")
            .short('P')
            .help("Parse and print AST instead of dissassembly")
        )
        .arg(
            Arg::new("bytecode")
            .short('d')
            .help("disassemble compiled bytecode (not implemented yet)")
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
    
    let source;
    let name;
    if let Some(s) = args.value_of("cmd") {
        source = ModuleSource::String(s.to_string());
        name = "<cmd>";
    } else if let Some(s) = args.value_of("source") {
        source = ModuleSource::File(PathBuf::from(s));
        name = s;
    } else if let Some(_s) = args.value_of("bytecode") {
        // TODO read compiled bytecode and go directly to disassembly
        unimplemented!()
    } else {
        println!("No input.");
        return;
    }
    
    println!("\nSphinx Version {}\n", version);

    if args.is_present("parse_only") {
        parse_and_print_ast(&args, name, &source);
        return;
    }
    
    
    // build module
    let build_result = build_module(&source);
    if let Err(error) = build_result {
        match error {
            BuildErrors::Source(error) => {
                println!("Error reading source: {}.", error);
            }
            
            BuildErrors::Syntax(errors) => {
                println!("Errors in file \"{}\":\n", name);
                frontend::print_source_errors(&source, &errors);
            }
            
            BuildErrors::Compile(errors) => {
                println!("Errors in file \"{}\":\n", name);
                frontend::print_source_errors(&source, &errors);
            }
        }
        return;
    }
    
    let build = build_result.unwrap();
    let symbols = build.symbols.values().flat_map(|table| table.symbols());
    let symbol_table = source.resolve_symbols(symbols);
    
    let dasm = {
        let dasm = Disassembler::new(&build.program)
            .with_symbols(&build.symbols);
        
        if let Ok(ref symbol_table) = symbol_table {
            dasm.with_symbol_table(symbol_table)
        } else {
            dasm
        }
    };
    
    println!("== \"{}\" ==", name);
    println!("{}", dasm);
}

fn parse_and_print_ast(_args: &ArgMatches, name: &str, source: &ModuleSource) {
    let source_text = match source.read_text() {
        Ok(source_text) => source_text,
        
        Err(error) => {
            println!("Error reading source: {}.", error);
            return;
        },
    };
    
    let mut interner = StringInterner::new();
    let parse_result = sphinx::parse_source(&mut interner, source_text);
    
    match parse_result {
        Err(errors) => {
            println!("Errors in file \"{}\":\n", name);
            frontend::print_source_errors(source, &errors);
        },
        Ok(ast) => println!("{:#?}", ast),
    }
}