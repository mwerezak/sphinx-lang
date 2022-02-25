// disable these until we have a working system
#![allow(dead_code)]
#![allow(unused_imports)]

#[macro_use]
extern crate lazy_static;

pub mod utils;
pub mod lexer;
pub mod parser;
pub mod language;
pub mod runtime;
pub mod frontend;
pub mod debug;