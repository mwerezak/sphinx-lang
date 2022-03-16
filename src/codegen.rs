#![allow(unused_variables)]

use crate::parser::stmt::{StmtMeta, Stmt};
use crate::parser::expr::{Expr, ExprMeta};
use crate::parser::primary::{Atom, Primary};
use crate::parser::assign::{Assignment, Declaration, LValue, DeclType};
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::strings::{StringInterner, InternSymbol};
use crate::debug::dasm::DebugSymbols;
use crate::debug::DebugSymbol;

pub mod chunk;
pub mod opcodes;
pub mod errors;

pub use opcodes::OpCode;
pub use chunk::{Chunk, ConstID};
pub use errors::{CompileResult, CompileError, ErrorKind};

use opcodes::*;
use chunk::{Constant, ChunkBuilder, UnloadedChunk};


#[derive(Debug)]
pub struct Program {
    pub bytecode: UnloadedChunk,
    pub symbols: DebugSymbols,
}

pub struct CodeGenerator {
    chunk: ChunkBuilder,
    symbols: DebugSymbols,
    errors: Vec<CompileError>,
}

impl CodeGenerator {
    pub fn new(strings: StringInterner) -> Self {
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
            let mut chunk = self.chunk;
            chunk.push_byte(OpCode::Return);
            
            let program = Program {
                bytecode: chunk.build(),
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
    
    fn make_const(&mut self, symbol: &DebugSymbol, value: Constant) -> CompileResult<ConstID> {
        self.chunk.push_const(value)
            .map_err(|error| error.with_symbol(*symbol))
    }
    
    fn emit_const(&mut self, symbol: &DebugSymbol, value: Constant) -> CompileResult<()> {
        let cid = self.make_const(symbol, value)?;
        
        if cid <= u8::MAX.into() {
            self.emit_instr_byte(symbol, OpCode::LoadConst, u8::try_from(cid).unwrap())
        } else {
            self.emit_instr_data(symbol, OpCode::LoadConst16, cid.to_le_bytes())
        }
    }
    
    fn compile_stmt(&mut self, symbol: &DebugSymbol, stmt: &Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Echo(expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_instr(symbol, OpCode::Inspect)
            },
            
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
            
            Expr::UnaryOp(op, expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_unary_op(symbol, op)
            },
            
            Expr::BinaryOp(op, exprs) => {
                let (ref lhs, ref rhs) = **exprs;
                self.compile_expr(symbol, lhs)?;
                self.compile_expr(symbol, rhs)?;
                self.emit_binary_op(symbol, op)
            },
            
            Expr::Declaration(decl) => self.compile_declaration(symbol, decl.decl, &decl.lhs, &decl.init),
            Expr::Assignment(assign) => self.compile_assignment(symbol, assign.op, &assign.lhs, &assign.rhs),
            
            Expr::Tuple(expr_list) => self.compile_tuple(symbol, expr_list),
            
            Expr::Block(label, suite) => unimplemented!(),
            
            Expr::FunctionDef(fundef) => unimplemented!(),
        }
    }
    
    fn compile_declaration(&mut self, symbol: &DebugSymbol, decl: DeclType, lvalue: &LValue, init: &Expr) -> CompileResult<()> {
        match lvalue {
            // TODO global/local name case
            LValue::Identifier(name) => self.compile_decl_global_name(symbol, decl, *name, &init),
            LValue::Attribute(target) => unimplemented!(),
            LValue::Index(target) => unimplemented!(),
            LValue::Tuple(target_list) => match init {
                
                Expr::Tuple(init_list) => {
                    if target_list.len() != init_list.len() {
                        return Err(CompileError::new("can't assign tuples of different lengths").with_symbol(*symbol))
                    }
                    
                    for (inner_lvalue, inner_expr) in target_list.iter().zip(init_list.iter()) {
                        let inner_symbol = inner_expr.debug_symbol();
                        let inner_init = inner_expr.variant();
                        self.compile_declaration(inner_symbol, decl, inner_lvalue, inner_init)?;
                    }
                    
                    Ok(())
                },
                
                _ => unimplemented!(), // dynamic declaration
            },
        }
    }
    
    fn compile_decl_global_name(&mut self, symbol: &DebugSymbol, decl: DeclType, name: InternSymbol, init: &Expr) -> CompileResult<()> {
        
        self.compile_expr(symbol, init)?;
        self.emit_const(symbol, Constant::from(name))?;
        
        match decl {
            DeclType::Immutable => self.emit_instr(symbol, OpCode::InsertGlobal),
            DeclType::Mutable => self.emit_instr(symbol, OpCode::InsertGlobalMut),
        }
    }
    
    fn compile_assignment(&mut self, symbol: &DebugSymbol, op: Option<BinaryOp>, lvalue: &LValue, rhs: &Expr) -> CompileResult<()> {
        match lvalue {
            // TODO global/local name case
            LValue::Identifier(name) => self.compile_assign_global_name(symbol, op, *name, &rhs),
            LValue::Attribute(target) => unimplemented!(),
            LValue::Index(target) => unimplemented!(),
            LValue::Tuple(..) if op.is_some() => {
                return Err(CompileError::new("can't use update-assigment when assigning to a tuple").with_symbol(*symbol))
            },
            
            LValue::Tuple(target_list) => match rhs {

                Expr::Tuple(rhs_list) => {
                    if target_list.len() != rhs_list.len() {
                        return Err(CompileError::new("can't assign tuples of different lengths").with_symbol(*symbol))
                    }
                    
                    for (inner_lvalue, inner_expr) in target_list.iter().zip(rhs_list.iter()) {
                        let inner_symbol = inner_expr.debug_symbol();
                        let inner_rhs = inner_expr.variant();
                        self.compile_assignment(inner_symbol, None, inner_lvalue, inner_rhs)?;
                    }
                    
                    Ok(())
                },
                
                _ => unimplemented!(), // dynamic declaration

            },
        }
    }
    
    fn compile_assign_global_name(&mut self, symbol: &DebugSymbol, op: Option<BinaryOp>, name: InternSymbol, rhs: &Expr) -> CompileResult<()> {
        
        if let Some(op) = op {
            // load current value
            self.emit_const(symbol, Constant::from(name))?;
            self.emit_instr(symbol, OpCode::LoadGlobal)?;
            
            // apply binary op
            self.compile_expr(symbol, rhs)?;
            self.emit_binary_op(symbol, &op)?;
            
        } else {
            self.compile_expr(symbol, rhs)?;
        }
        
        self.emit_const(symbol, Constant::from(name))?;
        self.emit_instr(symbol, OpCode::StoreGlobal)
    }
    
    fn compile_tuple(&mut self, symbol: &DebugSymbol, expr_list: &Box<[ExprMeta]>) -> CompileResult<()> {
        let len = u8::try_from(expr_list.len())
            .map_err(|_| CompileError::from(ErrorKind::TupleLengthLimit))?;
        
        for expr in expr_list.iter() {
            let inner_symbol = expr.debug_symbol();
            self.compile_expr(inner_symbol, expr.variant())?;
        }
        
        self.emit_instr_byte(symbol, OpCode::Tuple, len)
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
            
            Atom::Identifier(name) => {
                // TODO local/global name case
                self.emit_const(symbol, Constant::from(*name))?;
                self.emit_instr(symbol, OpCode::LoadGlobal)
            },
            
            // Atom::Self_ => unimplemented!(),
            // Atom::Super => unimplemented!(),
            
            Atom::Group(expr) => self.compile_expr(symbol, expr),
        }
    }
    
    fn compile_primary(&mut self, symbol: &DebugSymbol, primary: &Primary) -> CompileResult<()> {
        unimplemented!()
    }
    
    fn emit_unary_op(&mut self, symbol: &DebugSymbol, op: &UnaryOp) -> CompileResult<()> {
        match op {
            UnaryOp::Neg => self.emit_instr(symbol, OpCode::Neg),
            UnaryOp::Pos => self.emit_instr(symbol, OpCode::Pos),
            UnaryOp::Inv => self.emit_instr(symbol, OpCode::Inv),
            UnaryOp::Not => self.emit_instr(symbol, OpCode::Not),
        }
    }
    
    fn emit_binary_op(&mut self, symbol: &DebugSymbol, op: &BinaryOp) -> CompileResult<()> {
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


