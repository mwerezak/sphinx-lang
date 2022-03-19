use std::path::PathBuf;
use clap::{Command, Arg};

use sphinx_lang::frontend;
use sphinx_lang::{BuildErrors, build_module};
use sphinx_lang::source::{ModuleSource, SourceType};
use sphinx_lang::debug::symbol::DebugSymbolResolver;
use sphinx_lang::debug::dasm::Disassembler;

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
    
    let mut module = None;
    if let Some(s) = args.value_of("cmd") {
        let source = SourceType::String(s.to_string());
        module = Some(ModuleSource::new("<cmd>", source));
    } else if let Some(s) = args.value_of("source") {
        let source = SourceType::File(PathBuf::from(s));
        module = Some(ModuleSource::new(s, source));
    } else if let Some(_s) = args.value_of("bytecode") {
        // TODO read compiled bytecode and go directly to disassembly
        unimplemented!()
    }
    
    if module.is_none() {
        println!("No input.");
        return;
    }
    
    println!("\nSphinx Version {}\n", version);
    
    // build module
    let module = module.unwrap();
    let build_result = build_module(&module);
    if build_result.is_err() {
        match build_result.unwrap_err() {
            BuildErrors::Source(error) => {
                println!("Error reading source: {}.", error);
            }
            
            BuildErrors::Syntax(errors) => {
                println!("Errors in file \"{}\":\n", module.name());
                frontend::print_source_errors(&module, &errors);
            }
            
            BuildErrors::Compile(errors) => {
                println!("Errors in file \"{}\":\n", module.name());
                frontend::print_source_errors(&module, &errors);
            }
        }
        return;
    }
    
    let program = build_result.unwrap();
    let symbol_table = module.resolve_symbols(program.debug_symbols());
    
    let dasm = {
        let dasm = Disassembler::new(&program);
        
        if let Ok(ref symbol_table) = symbol_table {
            dasm.with_symbol_table(&symbol_table)
        } else {
            dasm
        }
    };
    
    println!("== \"{}\" ==", module.name());
    println!("{}", dasm);
}