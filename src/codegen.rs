#![allow(unused_variables)]

use log;
use std::iter;

use crate::language::FloatType;
use crate::parser::stmt::{StmtMeta, Stmt, Label, StmtList, ControlFlow};
use crate::parser::expr::{Expr, ExprMeta, Conditional};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeTag {
    Block,
    Loop,
    Branch,
    Function,
    Class,
}


#[derive(Debug)]
struct Scope {
    tag: ScopeTag,
    depth: usize,
    offset: Option<Offset>,
    locals: Vec<Local>,
    symbol: DebugSymbol,
}

impl Scope {
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
    
    // i.e., without the "nonlocal" keyword
    // this returns true if we can assign into the enclosing scope
    fn allow_enclosing_assignment(&self) -> bool {
        match self.tag {
            ScopeTag::Loop | ScopeTag::Branch => true,
            _ => false,
        }
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
    
    fn push_scope(&mut self, tag: ScopeTag, symbol: DebugSymbol) {
        let offset = self.local_scope().and_then(|scope| scope.last_offset());
        
        let scope = Scope {
            tag, symbol,
            depth: self.scopes.len(),
            offset,
            locals: Vec::new(),
        };
        self.scopes.push(scope);
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
    
    // find the nearest local in any scope
    fn resolve_local(&self, name: &InternSymbol) -> Option<&Local> {
        self.scopes.iter().rev().find_map(|scope| scope.find_local(name))
    }
    
    // find the nearest local in scopes that allow nonlocal assignment
    fn resolve_local_strict(&self, name: &InternSymbol) -> Option<&Local> {
        for scope in self.scopes.iter().rev() {
            let local = scope.find_local(name);
            if local.is_some() {
                return local;
            }
            
            if !scope.allow_enclosing_assignment() {
                break;
            }
        }
        
        None
    }
    
    // search scopes that would have been skipped by resolve_local_strict()
    fn resolve_nonlocal(&self, name: &InternSymbol) -> Option<&Local> {
        let mut is_local = true;
        for scope in self.scopes.iter().rev() {
            if is_local {
                is_local &= scope.allow_enclosing_assignment();
                continue;
            }
            
            let local = scope.find_local(name);
            if local.is_some() {
                return local;
            }
        }
        
        None
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
    
    fn emit_instr(&mut self, symbol: &DebugSymbol, opcode: OpCode) {
        debug_assert!(opcode.instr_len() == 1);
        self.symbols.push(symbol);
        self.chunk.push_byte(opcode);
    }
    
    fn emit_instr_byte(&mut self, symbol: &DebugSymbol, opcode: OpCode, byte: u8) {
        debug_assert!(opcode.instr_len() == 2);
        self.symbols.push(symbol);
        self.chunk.push_byte(opcode);
        self.chunk.push_byte(byte);
    }
    
    fn emit_instr_data(&mut self, symbol: &DebugSymbol, opcode: OpCode, bytes: &[u8]) {
        debug_assert!(opcode.instr_len() == 1 + bytes.len());
        self.symbols.push(symbol);
        self.chunk.push_byte(opcode);
        self.chunk.extend_bytes(&bytes);
    }
    
    // returns the offset to end of the instruction, for use with jump site patching
    fn emit_dummy_instr(&mut self, symbol: &DebugSymbol, opcode: OpCode) -> usize {
        self.symbols.push(symbol);
        
        let bytes = iter::repeat(OpCode::Nop).take(opcode.instr_len());
        for byte in bytes {
            self.chunk.push_byte(byte);
        }
        self.chunk.bytes().len()
    }
    
    // the given offset is the offset that is expected to be the *end* of the patched instruction
    fn patch_instr_data<const N: usize>(&mut self, end_offset: usize, opcode: OpCode, bytes: &[u8; N]) {
        debug_assert!(opcode.instr_len() == 1 + N);
        
        let offset = end_offset - opcode.instr_len();
        self.chunk.bytes_mut()[offset] = u8::from(opcode);
        self.chunk.patch_bytes(offset + 1, bytes);
    }
    
    fn make_const(&mut self, symbol: &DebugSymbol, value: Constant) -> CompileResult<ConstID> {
        self.chunk.push_const(value)
            .map_err(|error| error.with_symbol(*symbol))
    }
    
    fn emit_load_const(&mut self, symbol: &DebugSymbol, value: Constant) -> CompileResult<()> {
        let cid = self.make_const(symbol, value)?;
        
        if cid <= u8::MAX.into() {
            self.emit_instr_byte(symbol, OpCode::LoadConst, u8::try_from(cid).unwrap());
        } else {
            self.emit_instr_data(symbol, OpCode::LoadConst16, &cid.to_le_bytes());
        }
        Ok(())
    }
    
    fn emit_begin_scope(&mut self, tag: ScopeTag, symbol: &DebugSymbol) {
        self.state.push_scope(tag, *symbol);
    }
    
    fn emit_end_scope(&mut self) {
        let scope = self.state.pop_scope();
        let symbol = scope.symbol;
        
        // discard all the locals from the stack
        let mut discard = scope.locals.len();
        while discard > u8::MAX.into() {
            self.emit_instr_byte(&symbol, OpCode::DropLocals, u8::MAX);
            discard -= usize::from(u8::MAX);
        }
        
        if discard > 0 {
            self.emit_instr_byte(&symbol, OpCode::DropLocals, u8::try_from(discard).unwrap());
        }
    }
    
    fn compile_stmt(&mut self, symbol: &DebugSymbol, stmt: &Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Echo(expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_instr(symbol, OpCode::Inspect);
            },
            Stmt::Assert(expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_instr(symbol, OpCode::Assert);
            }
            
            Stmt::Expression(expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_instr(symbol, OpCode::Pop);
            },
        }
        Ok(())
    }
    
    fn compile_expr(&mut self, symbol: &DebugSymbol, expr: &Expr) -> CompileResult<()> {
        match expr {
            Expr::Atom(atom) => self.compile_atom(symbol, atom)?,
            
            Expr::Primary(primary) => unimplemented!(),
            
            Expr::UnaryOp(op, expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_unary_op(symbol, op);
            },
            
            Expr::BinaryOp(op, exprs) => {
                let (ref lhs, ref rhs) = **exprs;
                self.compile_expr(symbol, lhs)?;
                self.compile_expr(symbol, rhs)?;
                self.emit_binary_op(symbol, op);
            },
            
            Expr::Declaration(decl) => self.compile_declaration(symbol, (&**decl).into())?,
            Expr::Assignment(assign) => self.compile_assignment(symbol, (&**assign).into())?,
            
            Expr::Tuple(expr_list) => self.compile_tuple(symbol, expr_list)?,
            
            Expr::Block(label, stmt_list) => self.compile_block(symbol, label.as_ref(), stmt_list)?,
            Expr::IfExpr(cond) => self.compile_if_expr(symbol, cond)?,
            
            Expr::FunctionDef(fundef) => unimplemented!(),
        }
        Ok(())
    }
    
    fn compile_block(&mut self, symbol: &DebugSymbol, label: Option<&Label>, stmt_list: &StmtList) -> CompileResult<()> {
        
        self.compile_stmt_list(ScopeTag::Block, symbol, stmt_list)?;
        
        Ok(())
    }
    
    fn compile_if_expr(&mut self, symbol: &DebugSymbol, conditional: &Conditional) -> CompileResult<()> {
        debug_assert!(!conditional.branches().is_empty());
        
        // track the sites where we jump to the end, so we can patch them later
        let mut end_jump_sites = Vec::new();
        
        let else_branch = conditional.else_branch();
        
        // if there is no else branch, the last non-else branch won't have a jump to end, and won't pop the condition 
        let (last_branch, rest) = conditional.branches().split_last().unwrap();
        let iter_branches = rest.iter()
            .map(|branch| (false, branch))
            .chain(iter::once((true, last_branch)));
        
        for (is_last, branch) in iter_branches {
            let is_final_branch = is_last && else_branch.is_none();
            
            let cond_jump = 
                if is_final_branch { OpCode::JumpIfFalse } // keep condition value
                else { OpCode::PopJumpIfFalse };
            
            self.compile_expr(symbol, branch.cond_expr())?;
            let branch_jump_site = self.emit_dummy_instr(symbol, cond_jump);
            
            self.compile_stmt_list(ScopeTag::Branch, symbol, branch.suite())?;
            
            // site for the jump to end
            if !is_final_branch {
                let end_offset = self.emit_dummy_instr(symbol, OpCode::Jump);
                end_jump_sites.push(end_offset);
            }
            
            // target for the jump from the conditional of the now compiled branch
            let branch_target = self.chunk.bytes().len();
            let jump_offset = i16::try_from(branch_target - branch_jump_site).expect("exceeded max jump offset");
            self.patch_instr_data(branch_jump_site, cond_jump, &jump_offset.to_le_bytes());
        }
        
        if let Some(suite) = &conditional.else_branch() {
            
            self.compile_stmt_list(ScopeTag::Branch, symbol, suite)?;
            
        }
        
        // patch all of the end jump sites
        let end_target = self.chunk.bytes().len();
        for jump_site in end_jump_sites.iter() {
            
            let jump_offset = i16::try_from(end_target - jump_site).expect("exceeded max jump offset");
            self.patch_instr_data(*jump_site, OpCode::Jump, &jump_offset.to_le_bytes());
        }
        
        Ok(())
    }
    
    fn compile_stmt_list(&mut self, tag: ScopeTag, symbol: &DebugSymbol, stmt_list: &StmtList) -> CompileResult<()> {
        
        self.emit_begin_scope(tag, symbol);
        
        // compile stmt suite
        for stmt in stmt_list.suite().iter() {
            let inner_symbol = stmt.debug_symbol();
            self.compile_stmt(inner_symbol, stmt.variant())?;
        }
        
        // handle control flow
        let is_expr_block = matches!(tag, ScopeTag::Block | ScopeTag::Branch);
        match stmt_list.end_control() {
            None => if is_expr_block {
                self.emit_instr(symbol, OpCode::Nil); // implicit nil
            },
            
            Some(control) => match control {
                ControlFlow::Expression(expr) => if is_expr_block {
                    self.compile_expr(expr.debug_symbol(), expr.variant())?;
                }
                
                ControlFlow::Continue(label) => {
                    unimplemented!()
                }
                ControlFlow::Break(label, expr) => {
                    unimplemented!()
                }
                ControlFlow::Return(expr) => {
                    unimplemented!()
                }
            }
        }
        
        self.emit_end_scope();
        
        Ok(())
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
                    
                    // declarations are also expressions
                    let tuple_len = u8::try_from(init_list.len())
                        .map_err(|_| CompileError::from(ErrorKind::TupleLengthLimit).with_symbol(*symbol))?;
                    self.emit_instr_byte(symbol, OpCode::Tuple, tuple_len);
                    
                    Ok(())
                },
                
                _ => unimplemented!(), // dynamic declaration
            },
        }
    }
    
    fn compile_decl_local_name(&mut self, symbol: &DebugSymbol, decl: DeclType, name: InternSymbol, init: &Expr) -> CompileResult<()> {
        
        self.compile_expr(symbol, init)?;  // make sure to evaluate initializer first in order for shadowing to work
        
        self.state.insert_local(decl, name).map_err(|err| err.with_symbol(*symbol))?;
        self.emit_instr(symbol, OpCode::InsertLocal);
        Ok(())
    }
    
    fn compile_decl_global_name(&mut self, symbol: &DebugSymbol, decl: DeclType, name: InternSymbol, init: &Expr) -> CompileResult<()> {
        
        self.compile_expr(symbol, init)?;
        
        self.emit_load_const(symbol, Constant::from(name))?;
        match decl {
            DeclType::Immutable => self.emit_instr(symbol, OpCode::InsertGlobal),
            DeclType::Mutable => self.emit_instr(symbol, OpCode::InsertGlobalMut),
        }
        Ok(())
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
                    
                    // assignments are also expressions
                    let tuple_len = u8::try_from(rhs_list.len())
                        .map_err(|_| CompileError::from(ErrorKind::TupleLengthLimit).with_symbol(*symbol))?;
                    self.emit_instr_byte(symbol, OpCode::Tuple, tuple_len);
                    
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
            self.emit_binary_op(symbol, &op);
            
        } else {
            // normal assignment
            self.compile_expr(symbol, &assign.rhs)?;
            
        }
        
        // Generate assignment
        
        if let Some(local_scope) = self.state.local_scope() {
            
            // check if the name is found in the local scope...
            let local_offset = 
                if let Some(local) = self.state.resolve_local_strict(name) {
                    if local.decl != DeclType::Mutable {
                        return Err(CompileError::from(ErrorKind::CantAssignImmutable).with_symbol(*symbol));
                    }
                    Some(local.offset)
                } else { None };
            
            if let Some(offset) = local_offset {
                if let Ok(offset) = u8::try_from(offset) {
                    self.emit_instr_byte(symbol, OpCode::StoreLocal, offset);
                } else {
                    self.emit_instr_data(symbol, OpCode::StoreLocal16, &offset.to_le_bytes());
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
        self.emit_load_const(symbol, Constant::from(*name))?;
        self.emit_instr(symbol, OpCode::StoreGlobal);
        Ok(())
    }
    
    
    fn compile_tuple(&mut self, symbol: &DebugSymbol, expr_list: &[ExprMeta]) -> CompileResult<()> {
        let len = u8::try_from(expr_list.len())
            .map_err(|_| CompileError::from(ErrorKind::TupleLengthLimit).with_symbol(*symbol))?;
        
        for expr in expr_list.iter() {
            let inner_symbol = expr.debug_symbol();
            self.compile_expr(inner_symbol, expr.variant())?;
        }
        
        self.emit_instr_byte(symbol, OpCode::Tuple, len);
        Ok(())
    }
    
    fn compile_atom(&mut self, symbol: &DebugSymbol, atom: &Atom) -> CompileResult<()> {
        match atom {
            Atom::Nil => self.emit_instr(symbol, OpCode::Nil),
            Atom::EmptyTuple => self.emit_instr(symbol, OpCode::Empty),
            Atom::BooleanLiteral(true) => self.emit_instr(symbol, OpCode::True),
            Atom::BooleanLiteral(false) => self.emit_instr(symbol, OpCode::False),
            
            Atom::IntegerLiteral(value) => {
                if let Ok(value) = u8::try_from(*value) {
                    self.emit_instr_byte(symbol, OpCode::UInt8, value);
                } else if let Ok(value) = i8::try_from(*value) {
                    self.emit_instr_byte(symbol, OpCode::Int8, value.to_le_bytes()[0]);
                } else {
                    self.emit_load_const(symbol, Constant::from(*value))?;
                }
            },
            
            Atom::FloatLiteral(value) => {
                if FloatType::from(i8::MIN) <= *value && *value <= FloatType::from(i8::MAX) {
                    let value = *value as i8;
                    self.emit_instr_byte(symbol, OpCode::Float8, value.to_le_bytes()[0]);
                } else {
                    self.emit_load_const(symbol, Constant::from(*value))?;
                }
            },
            
            Atom::StringLiteral(value) => self.emit_load_const(symbol, Constant::from(*value))?,
            Atom::Identifier(name) => self.compile_name_lookup(symbol, name)?,
            
            // Atom::Self_ => unimplemented!(),
            // Atom::Super => unimplemented!(),
            
            Atom::Group(expr) => self.compile_expr(symbol, expr)?,
        }
        Ok(())
    }
    
    fn compile_name_lookup(&mut self, symbol: &DebugSymbol, name: &InternSymbol) -> CompileResult<()> {
        
        let local_offset = self.state.resolve_local(name).map(|local| local.offset);
        if let Some(offset) = local_offset {
            
            // Local Variable
            if let Ok(offset) = u8::try_from(offset) {
                self.emit_instr_byte(symbol, OpCode::LoadLocal, offset);
            } else {
                self.emit_instr_data(symbol, OpCode::LoadLocal16, &offset.to_le_bytes());
            }
        
        } else {
            
            // Global variable
            self.emit_load_const(symbol, Constant::from(*name))?;
            self.emit_instr(symbol, OpCode::LoadGlobal);
        }
        Ok(())
    }
    
    fn compile_primary(&mut self, symbol: &DebugSymbol, primary: &Primary) -> CompileResult<()> {
        unimplemented!()
    }
    
    fn emit_unary_op(&mut self, symbol: &DebugSymbol, op: &UnaryOp) {
        match op {
            UnaryOp::Neg => self.emit_instr(symbol, OpCode::Neg),
            UnaryOp::Pos => self.emit_instr(symbol, OpCode::Pos),
            UnaryOp::Inv => self.emit_instr(symbol, OpCode::Inv),
            UnaryOp::Not => self.emit_instr(symbol, OpCode::Not),
        }
    }
    
    fn emit_binary_op(&mut self, symbol: &DebugSymbol, op: &BinaryOp) {
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
