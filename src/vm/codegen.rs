use crate::parser::stmt::StmtMeta;
use crate::debug::dasm::DebugSymbols;
use crate::vm::Chunk;


struct CodeGenerator {
    program: Chunk,
    symbols: DebugSymbols,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            program: Chunk::new(),
            symbols: DebugSymbols::new()
        }
    }
    
    pub fn push_stmt(&mut self, stmt: StmtMeta) -> Result<(), CompileError> {
        unimplemented!()
    }
}


struct CompileError { }