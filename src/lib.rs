// disable these until we have a working system
#![allow(dead_code)]
#![allow(unused_imports)]

use std::io;

#[macro_use]
extern crate lazy_static;

pub mod utils;

pub mod source;
pub mod lexer;
pub mod parser;

pub mod language;
// pub mod interpreter;
pub mod codegen;
pub mod runtime;

pub mod frontend;
pub mod debug;


use source::{ModuleSource, ParseContext};
use parser::ParserError;
use codegen::{Program, CodeGenerator, CompileError};
use runtime::strings::StringInterner;

#[derive(Debug)]
pub enum BuildErrors {
    // depending on which stage the build failed
    Source(io::Error),
    Syntax(Box<[ParserError]>),
    Compile(Box<[CompileError]>),
}

pub fn build_module(module: &ModuleSource) -> Result<Program, BuildErrors> {
    let source_text = module.source_text();
    if source_text.is_err() {
        return Err(BuildErrors::Source(source_text.unwrap_err()));
    }
    
    // parsing
    let mut interner = StringInterner::new();
    let lexer_factory = language::create_default_lexer_rules();
    let mut parse_ctx = ParseContext::new(&lexer_factory, &mut interner);
    let source_text = source_text.unwrap();
    let parse_result = parse_ctx.parse_ast(source_text);
    
    if parse_result.is_err() {
        let errors = parse_result.unwrap_err().into_boxed_slice();
        return Err(BuildErrors::Syntax(errors));
    }
    
    // compilation
    let stmts = parse_result.unwrap();
    let codegen = CodeGenerator::with_strings(interner);
    let compile_result = codegen.compile_program(stmts.iter());
    
    if compile_result.is_err() {
        let errors = compile_result.unwrap_err().into_boxed_slice();
        return Err(BuildErrors::Compile(errors));
    }
    
    Ok(compile_result.unwrap())
}