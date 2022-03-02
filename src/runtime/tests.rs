#![cfg(test)]

use crate::debug::dasm::Disassembler;
use crate::runtime::variant::Variant;
use crate::runtime::bytecode::{OpCode, Chunk};


#[test]
fn dasm_test_opcode_const() {
    let mut chunk = Chunk::new();
    let id = chunk.push_const(Variant::Float(1.2));
    
    chunk.push_byte(OpCode::LoadConst);
    chunk.push_byte(id as u8);
    chunk.push_byte(OpCode::Return);
    
    let dasm = Disassembler::new(&chunk);
    println!("== dasm_test_opcode_const ==");
    println!("{}", dasm);
}