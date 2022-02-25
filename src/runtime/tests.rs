#![cfg(test)]

use crate::debug::dasm::Disassembler;
use crate::runtime::bytecode::{OpCode, Chunk};


#[test]
fn dasm_test_opcode_const() {
    let mut chunk = Chunk::new();
    chunk.push(OpCode::Const);
    chunk.push(23);
    chunk.push(OpCode::Return);
    
    let dasm = Disassembler::new(&chunk);
    println!("== dasm_test_opcode_const ==");
    println!("{}", dasm);
}