#![allow(unused_variables)]

use crate::parser::stmt::{StmtMeta, Stmt};
use crate::parser::expr::{Expr};
use crate::parser::primary::{Atom, Primary};
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::debug::dasm::DebugSymbols;
use crate::debug::symbol::DebugSymbol;
use crate::vm::Chunk;
use crate::vm::opcodes::*;


struct CodeGenerator {
    program: Chunk,
    symbols: DebugSymbols,
    // errors
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            program: Chunk::new(),
            symbols: DebugSymbols::new()
        }
    }
    
    pub fn push_stmt(&mut self, stmt: StmtMeta) -> CompileResult<()> {
        unimplemented!()
    }
    
    fn emit_instr(&mut self, symbol: &DebugSymbol, opcode: OpCode) {
        debug_assert!(opcode.instr_len() == 1);
        self.program.push_byte(opcode);
        self.symbols.push(symbol);
    }
    
    fn compile_stmt(&mut self, symbol: &DebugSymbol, stmt: &Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Echo(expr) => unimplemented!(),
            Stmt::Expression(expr) => unimplemented!(),
            Stmt::Continue(label) => unimplemented!(),
            Stmt::Break(label, expr) => unimplemented!(),
            Stmt::Return(expr) => unimplemented!(),
        }
    }
    
    fn compile_expr(&mut self, symbol: &DebugSymbol, expr: &Expr) -> CompileResult<()> {
        match expr {
            Expr::Atom(atom) => unimplemented!(),
            
            Expr::Primary(primary) => unimplemented!(),
            
            Expr::UnaryOp(op, expr) => unimplemented!(),
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
            UnaryOp::Neg => unimplemented!(),
            UnaryOp::Pos => unimplemented!(),
            UnaryOp::Inv => unimplemented!(),
            UnaryOp::Not => unimplemented!(),
        }
    }
}


struct Scope {
    // locals
}


type CompileResult<T> = Result<T, CompileError>;

struct CompileError {
    // undefined locals, etc
}