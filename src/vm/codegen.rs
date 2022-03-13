#![allow(unused_variables)]

use crate::parser::stmt::{StmtMeta, Stmt};
use crate::parser::expr::{Expr};
use crate::parser::primary::{Atom, Primary};
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::debug::dasm::DebugSymbols;
use crate::debug::symbol::DebugSymbol;
use crate::vm::Chunk;
use crate::vm::opcodes::*;


pub struct Program {
    pub bytecode: Chunk,
    pub symbols: DebugSymbols,
}

impl Default for Program {
    fn default() -> Self {
        Program {
            bytecode: Chunk::new(),
            symbols: DebugSymbols::new(),
        }
    }
}

pub struct CodeGenerator {
    program: Program,
    errors: Vec<CompileError>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            program: Program::default(),
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
            Ok(self.program)
        } else {
            Err(self.errors)
        }
    }
    
    pub fn push_stmt(&mut self, stmt: &StmtMeta) {
        if let Err(error) = self.compile_stmt(stmt.debug_symbol(), stmt.variant()) {
            self.errors.push(error);
        }
    }
    
    fn emit_instr(&mut self, symbol: &DebugSymbol, opcode: OpCode) {
        debug_assert!(opcode.instr_len() == 1);
        self.program.bytecode.push_byte(opcode);
        self.program.symbols.push(symbol);
    }
    
    fn compile_stmt(&mut self, symbol: &DebugSymbol, stmt: &Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Echo(expr) => unimplemented!(),
            Stmt::Expression(expr) => self.compile_expr(symbol, expr),
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
                unimplemented!()
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
            Atom::IntegerLiteral(value) => unimplemented!(),
            Atom::FloatLiteral(value) => unimplemented!(),
            Atom::StringLiteral(sym) => unimplemented!(),
            
            Atom::Identifier(name) => unimplemented!(),
            
            Atom::Self_ => unimplemented!(),
            Atom::Super => unimplemented!(),
            
            Atom::Group(expr) => unimplemented!(),
        }
        Ok(())
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
        Ok(())
    }
}


struct Scope {
    // locals
}


pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub struct CompileError {
    // undefined locals, etc
}