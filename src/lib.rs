// disable these until we have a working system
#![allow(dead_code)]
// #![allow(unused_imports)]
#![feature(ptr_metadata)]

use std::io;

#[macro_use]
mod macros;

pub mod utils;

pub mod source;
pub mod lexer;
pub mod parser;

pub mod language;
pub mod codegen;
pub mod runtime;
pub mod builtins;

pub mod frontend;
pub mod debug;


use source::{SourceText, ModuleSource, ParseContext};
use parser::ParserError;
use parser::stmt::StmtMeta;
use codegen::{CompiledProgram, Compiler, CompileError};
use runtime::strings::StringInterner;

#[derive(Debug)]
pub enum BuildErrors {
    // depending on which stage the build failed
    Source(io::Error),
    Syntax(Box<[ParserError]>),
    Compile(Box<[CompileError]>),
}

pub fn build_module(source: &ModuleSource) -> Result<CompiledProgram, BuildErrors> {
    let source_text = source.read_text()
        .map_err(BuildErrors::Source)?;
    
    build_source(source_text)
}

pub fn build_source(source_text: SourceText) -> Result<CompiledProgram, BuildErrors> {
    let mut interner = StringInterner::new();
    
    // parsing
    let parse_result = parse_source(&mut interner, source_text);
    
    if let Err(errors) = parse_result {
        return Err(BuildErrors::Syntax(errors.into_boxed_slice()));
    }
    
    // compilation
    let compile_result = compile_ast(interner, parse_result.unwrap());
    
    if let Err(errors) = compile_result {
        return Err(BuildErrors::Compile(errors.into_boxed_slice()));
    }
    
    Ok(compile_result.unwrap())
}



/// Produce AST from SourceText
pub fn parse_source(interner: &mut StringInterner, source_text: SourceText) -> Result<Vec<StmtMeta>, Vec<ParserError>> {
    let lexer_factory = language::create_default_lexer_rules();
    let mut parse_ctx = ParseContext::new(&lexer_factory, interner);
    
    parse_ctx.parse_ast(source_text)
}

/// Produce bytecode from AST
pub fn compile_ast(interner: StringInterner, ast: Vec<StmtMeta>) -> Result<CompiledProgram, Vec<CompileError>> {
    let compiler = Compiler::new(interner);
    compiler.compile_program(ast.iter())
}


pub fn print_build_errors(errors: &BuildErrors, source: &ModuleSource) {
    match errors {
        BuildErrors::Source(error) => {
            println!("Error reading source: {}.", error);
        }
        
        BuildErrors::Syntax(errors) => {
            println!("Errors in {}:\n", source);
            frontend::print_source_errors(source, errors);
        }
        
        BuildErrors::Compile(errors) => {
            println!("Errors in {}:\n", source);
            frontend::print_source_errors(source, errors);
        }
    }
    
}