#![allow(unused_variables)]

use crate::parser::stmt::{StmtMeta, Stmt, Label};
use crate::parser::expr::{Expr, ExprMeta};
use crate::parser::primary::{Atom, Primary};
use crate::parser::assign::{Assignment, Declaration, LValue, DeclType};
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::strings::{StringInterner, InternSymbol};
use crate::runtime::vm::LocalIndex;
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




// Output container

#[derive(Debug)]
pub struct Program {
    pub bytecode: UnloadedChunk,
    pub symbols: DebugSymbols,
}

// Helpers

struct AssignmentRef<'a> {
    lhs: &'a LValue,
    op: Option<BinaryOp>,
    rhs: &'a Expr,
    nonlocal: bool,
}

impl<'a> From<&'a Assignment> for AssignmentRef<'a> {
    fn from(assign: &'a Assignment) -> Self {
        Self {
            lhs: &assign.lhs,
            op: assign.op,
            rhs: &assign.rhs,
            nonlocal: assign.nonlocal,
        }
    }
}

struct DeclarationRef<'a> {
    decl: DeclType,
    lhs: &'a LValue,
    init: &'a Expr,
}

impl<'a> From<&'a Declaration> for DeclarationRef<'a> {
    fn from(decl: &'a Declaration) -> Self {
        Self {
            decl: decl.decl,
            lhs: &decl.lhs,
            init: &decl.init,
        }
    }
}


// Scope Tracking

type Offset = LocalIndex;

#[derive(Debug)]
struct Local {
    decl: DeclType,
    name: InternSymbol,
    offset: Offset,
}

#[derive(Debug)]
struct Scope {
    depth: usize,
    offset: Option<Offset>,
    locals: Vec<Local>,
    symbol: DebugSymbol,
}

impl Scope {
    fn new(symbol: DebugSymbol, depth: usize, offset: Option<Offset>) -> Self {
        Self { symbol, depth, offset, locals: Vec::new() }
    }
    
    fn last_offset(&self) -> Option<Offset> {
        self.locals.last().map_or(self.offset, |local| Some(local.offset))
    }
    
    fn find_local(&self, name: &InternSymbol) -> Option<&Local> {
        self.locals.iter().find(|local| local.name == *name)
    }
    
    fn find_local_mut(&mut self, name: &InternSymbol) -> Option<&mut Local> {
        self.locals.iter_mut().find(|local| local.name == *name)
    }
    
    fn push_local(&mut self, decl: DeclType, name: InternSymbol) -> CompileResult<&Local> {
        let offset = self.last_offset().map_or(
            Ok(0),
            |offset| offset.checked_add(1)
                .ok_or_else(|| CompileError::from(ErrorKind::LocalVariableLimit))
        )?;
        
        let local = Local {
            decl, name, offset,
        };
        self.locals.push(local);
        Ok(self.locals.last().unwrap())
    }
}

#[derive(Debug)]
struct CompilerState {
    scopes: Vec<Scope>,
}

impl CompilerState {
    fn new() -> Self {
        Self { scopes: Vec::new() }
    }
    
    fn is_global_scope(&self) -> bool {
        self.scopes.is_empty()
    }
    
    fn local_scope(&self) -> Option<&Scope> {
        self.scopes.last()
    }
    
    fn local_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
    }
    
    fn push_scope(&mut self, symbol: DebugSymbol) {
        let offset = self.local_scope().and_then(|scope| scope.last_offset());
        self.scopes.push(Scope::new(symbol, self.scopes.len(), offset));
    }
    
    fn pop_scope(&mut self) -> Scope {
        self.scopes.pop().expect("pop global scope")
    }
    
    fn insert_local(&mut self, decl: DeclType, name: InternSymbol) -> CompileResult<()> {
        let local_scope = self.local_scope_mut().expect("insert local in global scope");
        
        // see if this local already exists in the current scope
        if let Some(mut local) = local_scope.find_local_mut(&name) {
            (*local).decl = decl; // redeclare with new mutability
        } else {
            local_scope.push_local(decl, name)?;
        }
        Ok(())
    }
    
    fn resolve_local(&mut self, name: &InternSymbol) -> Option<&Local> {
        self.scopes.iter().rev().find_map(|scope| scope.find_local(name))
    }
}


// Code Generator

pub struct CodeGenerator {
    chunk: ChunkBuilder,
    symbols: DebugSymbols,
    state: CompilerState,
    errors: Vec<CompileError>,
}

impl CodeGenerator {
    pub fn new(strings: StringInterner) -> Self {
        CodeGenerator {
            chunk: ChunkBuilder::with_strings(strings),
            symbols: DebugSymbols::default(),
            state: CompilerState::new(),
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
    
    fn emit_begin_scope(&mut self, symbol: &DebugSymbol) -> CompileResult<()> {
        self.state.push_scope(*symbol);
        Ok(())
    }
    
    fn emit_end_scope(&mut self) -> CompileResult<()> {
        let scope = self.state.pop_scope();
        let symbol = scope.symbol;
        
        // discard all the locals from the stack
        let mut discard = scope.locals.len();
        while discard > u8::MAX.into() {
            self.emit_instr_byte(&symbol, OpCode::DropLocals, u8::MAX)?;
            discard -= usize::from(u8::MAX);
        }
        
        self.emit_instr_byte(&symbol, OpCode::DropLocals, u8::try_from(discard).unwrap())?;
        Ok(())
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
            
            Expr::Declaration(decl) => self.compile_declaration(symbol, (&**decl).into()),
            Expr::Assignment(assign) => self.compile_assignment(symbol, (&**assign).into()),
            
            Expr::Tuple(expr_list) => self.compile_tuple(symbol, expr_list),
            
            Expr::Block(label, suite) => self.compile_block(symbol, label.as_ref(), suite),
            
            Expr::FunctionDef(fundef) => unimplemented!(),
        }
    }
    
    fn compile_block(&mut self, symbol: &DebugSymbol, label: Option<&Label>, suite: &[StmtMeta]) -> CompileResult<()> {
        self.emit_begin_scope(symbol)?;
        
        // TODO control flow
        for stmt in suite.iter() {
            let inner_symbol = stmt.debug_symbol();
            self.compile_stmt(inner_symbol, stmt.variant())?;
        }
        
        self.emit_end_scope()?;
        
        self.emit_instr(symbol, OpCode::Nil) // implicit nil if we don't break out of block
    }
    
    fn compile_declaration(&mut self, symbol: &DebugSymbol, decl: DeclarationRef) -> CompileResult<()> {
        match &decl.lhs {
            LValue::Identifier(name) => if self.state.is_global_scope() {
                self.compile_decl_global_name(symbol, decl.decl, *name, &decl.init)
            } else {
                self.compile_decl_local_name(symbol, decl.decl, *name, &decl.init)
            },
            
            LValue::Attribute(target) => unimplemented!(),
            LValue::Index(target) => unimplemented!(),
            
            LValue::Tuple(target_list) => match &decl.init {
                
                Expr::Tuple(init_list) => {
                    if target_list.len() != init_list.len() {
                        return Err(CompileError::new("can't assign tuples of different lengths").with_symbol(*symbol))
                    }
                    
                    for (inner_lhs, inner_expr) in target_list.iter().zip(init_list.iter()) {
                        let inner_symbol = inner_expr.debug_symbol();
                        let inner_init = inner_expr.variant();
                        
                        let inner_decl = DeclarationRef {
                            decl: decl.decl,
                            lhs: inner_lhs,
                            init: inner_init,
                        };
                        
                        self.compile_declaration(inner_symbol, inner_decl)?;
                    }
                    
                    Ok(())
                },
                
                _ => unimplemented!(), // dynamic declaration
            },
        }
    }
    
    fn compile_decl_local_name(&mut self, symbol: &DebugSymbol, decl: DeclType, name: InternSymbol, init: &Expr) -> CompileResult<()> {
        
        self.compile_expr(symbol, init)?;  // make sure to evaluate initializer first in order for shadowing to work
        
        self.state.insert_local(decl, name).map_err(|err| err.with_symbol(*symbol))?;
        self.emit_instr(symbol, OpCode::InsertLocal)
    }
    
    fn compile_decl_global_name(&mut self, symbol: &DebugSymbol, decl: DeclType, name: InternSymbol, init: &Expr) -> CompileResult<()> {
        
        self.compile_expr(symbol, init)?;
        
        self.emit_const(symbol, Constant::from(name))?;
        match decl {
            DeclType::Immutable => self.emit_instr(symbol, OpCode::InsertGlobal),
            DeclType::Mutable => self.emit_instr(symbol, OpCode::InsertGlobalMut),
        }
    }
    
    fn compile_assignment(&mut self, symbol: &DebugSymbol, assign: AssignmentRef) -> CompileResult<()> {
        match assign.lhs {
            // LValue::Identifier(name) if self.state.is_global_scope() => self.compile_assign_global_name(symbol, op, *name, &rhs),
            LValue::Identifier(name) => self.compile_assign_identifier(symbol, name, assign),
            
            LValue::Attribute(target) => unimplemented!(),
            LValue::Index(target) => unimplemented!(),
            
            LValue::Tuple(..) if assign.op.is_some() => {
                return Err(CompileError::new("can't use update-assigment when assigning to a tuple").with_symbol(*symbol))
            },
            
            LValue::Tuple(target_list) => match assign.rhs {

                Expr::Tuple(rhs_list) => {
                    if target_list.len() != rhs_list.len() {
                        return Err(CompileError::new("can't assign tuples of different lengths").with_symbol(*symbol))
                    }
                    
                    for (inner_lhs, inner_expr) in target_list.iter().zip(rhs_list.iter()) {
                        let inner_symbol = inner_expr.debug_symbol();
                        let inner_rhs = inner_expr.variant();
                        
                        let inner_assign = AssignmentRef {
                            lhs: inner_lhs,
                            op: None,
                            rhs: inner_rhs,
                            nonlocal: assign.nonlocal,
                        };
                        
                        self.compile_assignment(inner_symbol, inner_assign)?;
                    }
                    
                    Ok(())
                },
                
                _ => unimplemented!(), // dynamic declaration

            },
        }
    }
    
    fn compile_assign_identifier(&mut self, symbol: &DebugSymbol, name: &InternSymbol, assign: AssignmentRef) -> CompileResult<()> {
        
        // Compile RHS
        
        if let Some(op) = assign.op {
            // update assignment
            self.compile_name_lookup(symbol, name)?;
            self.compile_expr(symbol, &assign.rhs)?;
            self.emit_binary_op(symbol, &op)?;
            
        } else {
            // normal assignment
            self.compile_expr(symbol, &assign.rhs)?;
            
        }
        
        // Generate assignment
        
        if let Some(local_scope) = self.state.local_scope() {
            
            // check if the name is found in the local scope...
            let local_offset = 
                if let Some(local) = self.state.resolve_local(name) {
                    if local.decl != DeclType::Mutable {
                        return Err(CompileError::from(ErrorKind::CantAssignImmutable).with_symbol(*symbol));
                    }
                    Some(local.offset)
                } else { None };
            
            if let Some(offset) = local_offset {
                if let Ok(offset) = u8::try_from(offset) {
                    self.emit_instr_byte(symbol, OpCode::StoreLocal, offset)?;
                } else {
                    self.emit_instr_data(symbol, OpCode::StoreLocal16, offset.to_le_bytes())?;
                }
                return Ok(());
            }
            
            // otherwise search enclosing scopes...
            
            if !assign.nonlocal {
                return Err(CompileError::from(ErrorKind::CantAssignNonLocal).with_symbol(*symbol));
            }
            
            unimplemented!();
            
            // return Ok(());
        }
        
        // ...finally, try to assign global
        self.emit_const(symbol, Constant::from(*name))?;
        self.emit_instr(symbol, OpCode::StoreGlobal)
    }
    
    
    fn compile_tuple(&mut self, symbol: &DebugSymbol, expr_list: &[ExprMeta]) -> CompileResult<()> {
        let len = u8::try_from(expr_list.len())
            .map_err(|_| CompileError::from(ErrorKind::TupleLengthLimit).with_symbol(*symbol))?;
        
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
            
            Atom::Identifier(name) => self.compile_name_lookup(symbol, name),
            
            // Atom::Self_ => unimplemented!(),
            // Atom::Super => unimplemented!(),
            
            Atom::Group(expr) => self.compile_expr(symbol, expr),
        }
    }
    
    fn compile_name_lookup(&mut self, symbol: &DebugSymbol, name: &InternSymbol) -> CompileResult<()> {
        
        let local_offset = self.state.resolve_local(name).map(|local| local.offset);
        if let Some(offset) = local_offset {
            
            // Local Variable
            if let Ok(offset) = u8::try_from(offset) {
                self.emit_instr_byte(symbol, OpCode::LoadLocal, offset)
            } else {
                self.emit_instr_data(symbol, OpCode::LoadLocal16, offset.to_le_bytes())
            }
        
        } else {
            
            // Global variable
            self.emit_const(symbol, Constant::from(*name))?;
            self.emit_instr(symbol, OpCode::LoadGlobal)
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
