#![allow(unused_variables)]

use crate::parser::stmt::{StmtMeta, Stmt};
use crate::parser::expr::{Expr};
use crate::parser::primary::{Atom, Primary};
use crate::runtime::Variant;
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::strings::StringInterner;
use crate::debug::dasm::DebugSymbols;
use crate::debug::DebugSymbol;

pub mod chunk;
pub mod opcodes;
pub mod errors;

pub use opcodes::OpCode;
pub use chunk::{Chunk, ConstID};

use opcodes::*;
use chunk::{Constant, ChunkBuilder, UnloadedChunk};
use errors::{CompileResult, CompileError};


pub struct Program {
    bytecode: UnloadedChunk,
    symbols: DebugSymbols,
}

impl Program {
    pub fn bytecode(&self) -> &UnloadedChunk { &self.bytecode }
    pub fn symbols(&self) -> &DebugSymbols { &self.symbols }
}

pub struct CodeGenerator {
    chunk: ChunkBuilder,
    symbols: DebugSymbols,
    errors: Vec<CompileError>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            chunk: ChunkBuilder::default(),
            symbols: DebugSymbols::default(),
            errors: Vec::new(),
        }
    }
    
    pub fn with_strings(strings: StringInterner) -> Self {
        CodeGenerator {
            chunk: ChunkBuilder::with_strings(strings),
            symbols: DebugSymbols::default(),
            errors: Vec::new(),
        }
    }
    
    pub fn compile_program<'a>(mut self, program: impl Iterator<Item=&'a StmtMeta>) -> Result<Program, Vec<CompileError>> {
        for stmt in program {
            self.push_stmt(stmt);
        }
        self.finish()
    }
    
    pub fn finish(self) -> Result<Program, Vec<CompileError>> {
        if self.errors.is_empty() {
            let program = Program {
                bytecode: self.chunk.build(),
                symbols: self.symbols,
            };
            Ok(program)
        } else {
            Err(self.errors)
        }
    }
    
    pub fn push_stmt(&mut self, stmt: &StmtMeta) {
        if let Err(error) = self.compile_stmt(stmt.debug_symbol(), stmt.variant()) {
            self.errors.push(error);
        }
    }
    
    fn emit_instr(&mut self, symbol: &DebugSymbol, opcode: OpCode) -> CompileResult<()> {
        debug_assert!(opcode.instr_len() == 1);
        self.symbols.push(symbol);
        self.chunk.push_byte(opcode);
        Ok(())
    }
    
    fn emit_instr_byte(&mut self, symbol: &DebugSymbol, opcode: OpCode, byte: u8) -> CompileResult<()> {
        debug_assert!(opcode.instr_len() == 2);
        self.symbols.push(symbol);
        self.chunk.push_byte(opcode);
        self.chunk.push_byte(byte);
        Ok(())
    }
    
    fn emit_instr_data<const N: usize>(&mut self, symbol: &DebugSymbol, opcode: OpCode, bytes: [u8; N]) -> CompileResult<()> {
        debug_assert!(opcode.instr_len() == 1 + N);
        self.symbols.push(symbol);
        self.chunk.push_byte(opcode);
        self.chunk.extend_bytes(&bytes);
        Ok(())
    }
    
    fn emit_const(&mut self, symbol: &DebugSymbol, value: Constant) -> CompileResult<()> {
        let cid = self.chunk.push_const(value)
            .map_err(|error| error.with_symbol(*symbol))?;
        
        if cid <= u8::MAX.into() {
            self.emit_instr_byte(symbol, OpCode::LoadConst, u8::try_from(cid).unwrap())
        } else {
            self.emit_instr_data(symbol, OpCode::LoadConst16, cid.to_le_bytes())
        }
    }
    
    fn compile_stmt(&mut self, symbol: &DebugSymbol, stmt: &Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Echo(expr) => unimplemented!(),
            Stmt::Expression(expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_instr(symbol, OpCode::Pop)
            },
            Stmt::Continue(label) => unimplemented!(),
            Stmt::Break(label, expr) => unimplemented!(),
            Stmt::Return(expr) => unimplemented!(),
        }
    }
    
    fn compile_expr(&mut self, symbol: &DebugSymbol, expr: &Expr) -> CompileResult<()> {
        match expr {
            Expr::Atom(atom) => self.compile_atom(symbol, atom),
            
            Expr::Primary(primary) => unimplemented!(),
            
            Expr::UnaryOp(op, expr) => self.compile_unary_op(symbol, op, expr),
            Expr::BinaryOp(op, exprs) => {
                let (ref lhs, ref rhs) = **exprs;
                self.compile_binary_op(symbol, op, lhs, rhs)
            },
            
            Expr::Assignment(assignment) => unimplemented!(),
            Expr::Declaration(declaration) => unimplemented!(),
            
            Expr::Tuple(expr_list) => unimplemented!(),
            Expr::ObjectCtor(ctor) => unimplemented!(),
            
            Expr::Block(label, suite) => unimplemented!(),
            
            Expr::FunctionDef(fundef) => unimplemented!(),
        }
    }
    
    fn compile_atom(&mut self, symbol: &DebugSymbol, atom: &Atom) -> CompileResult<()> {
        match atom {
            Atom::Nil => self.emit_instr(symbol, OpCode::Nil),
            Atom::EmptyTuple => self.emit_instr(symbol, OpCode::Empty),
            Atom::BooleanLiteral(true) => self.emit_instr(symbol, OpCode::True),
            Atom::BooleanLiteral(false) => self.emit_instr(symbol, OpCode::False),
            Atom::IntegerLiteral(value) => self.emit_const(symbol, Constant::from(*value)),
            Atom::FloatLiteral(value) => self.emit_const(symbol, Constant::from(*value)),
            Atom::StringLiteral(value) => self.emit_const(symbol, Constant::from(*value)),
            
            Atom::Identifier(name) => unimplemented!(),
            
            // Atom::Self_ => unimplemented!(),
            // Atom::Super => unimplemented!(),
            
            Atom::Group(expr) => self.compile_expr(symbol, expr),
        }
    }
    
    fn compile_primary(&mut self, symbol: &DebugSymbol, primary: &Primary) -> CompileResult<()> {
        unimplemented!()
    }
    
    fn compile_unary_op(&mut self, symbol: &DebugSymbol, op: &UnaryOp, expr: &Expr) -> CompileResult<()> {
        self.compile_expr(symbol, expr)?;
        
        match op {
            UnaryOp::Neg => self.emit_instr(symbol, OpCode::Neg),
            UnaryOp::Pos => self.emit_instr(symbol, OpCode::Pos),
            UnaryOp::Inv => self.emit_instr(symbol, OpCode::Inv),
            UnaryOp::Not => self.emit_instr(symbol, OpCode::Not),
        }
    }
    
    fn compile_binary_op(&mut self, symbol: &DebugSymbol, op: &BinaryOp, lhs: &Expr,  rhs: &Expr) -> CompileResult<()> {
        self.compile_expr(symbol, lhs)?;
        self.compile_expr(symbol, rhs)?;
        
        match op {
            BinaryOp::Logical(logic) => unimplemented!(),
            
            BinaryOp::Arithmetic(op) => match op {
                Arithmetic::Mul => self.emit_instr(symbol, OpCode::Mul),
                Arithmetic::Div => self.emit_instr(symbol, OpCode::Div),
                Arithmetic::Mod => self.emit_instr(symbol, OpCode::Mod),
                Arithmetic::Add => self.emit_instr(symbol, OpCode::Add),
                Arithmetic::Sub => self.emit_instr(symbol, OpCode::Sub),
            },
            
            BinaryOp::Bitwise(op) => match op {
                Bitwise::And => self.emit_instr(symbol, OpCode::And),
                Bitwise::Xor => self.emit_instr(symbol, OpCode::Xor),
                Bitwise::Or  => self.emit_instr(symbol, OpCode::Or),
            },
            
            BinaryOp::Shift(op) => match op {
                Shift::Left  => self.emit_instr(symbol, OpCode::Shl),
                Shift::Right => self.emit_instr(symbol, OpCode::Shr),
            },
            
            BinaryOp::Comparison(op) => match op {
                Comparison::LT => self.emit_instr(symbol, OpCode::LT),
                Comparison::GT => self.emit_instr(symbol, OpCode::GT),
                Comparison::LE => self.emit_instr(symbol, OpCode::LE),
                Comparison::GE => self.emit_instr(symbol, OpCode::GE),
                Comparison::EQ => self.emit_instr(symbol, OpCode::EQ),
                Comparison::NE => self.emit_instr(symbol, OpCode::NE),
            },
        }
    }
}


struct Scope {
    // locals
}


