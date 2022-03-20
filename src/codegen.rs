#![allow(unused_variables)]

use log;
use std::iter;
use std::mem;

use crate::language::FloatType;
use crate::parser::stmt::{StmtMeta, Stmt, Label, StmtList, ControlFlow};
use crate::parser::expr::{Expr, ExprMeta, ExprBlock, ConditionalBranch};
use crate::parser::primary::{Atom, Primary};
use crate::parser::lvalue::{Assignment, Declaration, LValue, DeclType};
use crate::parser::fundefs::{FunctionDef, SignatureDef, RequiredDef, DefaultDef};
use crate::runtime::vm::LocalIndex;
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::types::function::{Function, Signature, Parameter};
use crate::runtime::strings::{StringInterner, InternSymbol};
use crate::debug::DebugSymbol;
use crate::debug::dasm::{ChunkSymbols, DebugSymbolsRLE};

pub mod chunk;
pub mod opcodes;
pub mod errors;

pub use opcodes::OpCode;
pub use chunk::{UnloadedProgram, Program, ChunkID, ConstID, Constant};
pub use errors::{CompileResult, CompileError, ErrorKind};

use opcodes::*;
use chunk::{ChunkBuilder, ChunkInfo, ChunkBuf};


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


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum JumpOffset {
    Short(i16),
    Long(i32),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Jump {
    Uncond,
    IfFalse,
    IfTrue,
    PopIfFalse,
    PopIfTrue,
}

impl Jump {
    pub const fn dummy_width(&self) -> usize {
        get_jump_opcode(Jump::Uncond, JumpOffset::Short(0)).instr_len()
    }
}

const fn get_jump_opcode(jump: Jump, offset: JumpOffset) -> OpCode {
    match (jump, offset) {
        (Jump::Uncond,  JumpOffset::Short(..)) => OpCode::Jump,
        (Jump::IfFalse, JumpOffset::Short(..)) => OpCode::JumpIfFalse,
        (Jump::IfTrue,  JumpOffset::Short(..)) => OpCode::JumpIfTrue,
        (Jump::Uncond,  JumpOffset::Long(..))  => unimplemented!(),
        (Jump::IfFalse, JumpOffset::Long(..))  => unimplemented!(),
        (Jump::IfTrue,  JumpOffset::Long(..))  => unimplemented!(),
        
        (Jump::PopIfFalse, JumpOffset::Short(..))  => OpCode::PopJumpIfFalse,
        (Jump::PopIfTrue,  JumpOffset::Short(..))  => OpCode::PopJumpIfTrue,
        (Jump::PopIfFalse, JumpOffset::Long(..))   => unimplemented!(),
        (Jump::PopIfTrue,  JumpOffset::Long(..))   => unimplemented!(),
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
    symbol: Option<DebugSymbol>,
}

impl Scope {
    fn debug_symbol(&self) -> Option<&DebugSymbol> {
        self.symbol.as_ref()
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
struct ScopeTracker {
    scopes: Vec<Scope>,
}

impl ScopeTracker {
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
    
    fn push_scope(&mut self, tag: ScopeTag, symbol: Option<&DebugSymbol>) {
        let offset = self.local_scope().and_then(|scope| scope.last_offset());
        
        let scope = Scope {
            tag, 
            symbol: symbol.map(|sym| *sym),
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
    
    fn iter_scopes(&self) -> impl Iterator<Item=&Scope> {
        self.scopes.iter().rev()
    }
    
    // find the nearest local in any scope
    fn resolve_local(&self, name: &InternSymbol) -> Option<&Local> {
        self.scopes.iter().rev().find_map(|scope| scope.find_local(name))
    }
    
    // find the nearest local in scopes that allow nonlocal assignment
    fn resolve_local_strict(&self, name: &InternSymbol) -> Option<&Local> {
        for scope in self.iter_scopes() {
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
        for scope in self.iter_scopes() {
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



/// Output container
#[derive(Debug)]
pub struct CompiledProgram {
    pub program: UnloadedProgram,
    pub symbols: ChunkSymbols,
}

// Code Generator

const CHUNK_MAIN: ChunkID = 0;

pub struct Compiler {
    builder: ChunkBuilder,
    scope: ScopeTracker,
    errors: Vec<CompileError>,
    symbols: ChunkSymbols,
}

impl Compiler {
    pub fn new(strings: StringInterner) -> Self {
        let mut compiler = Compiler {
            builder: ChunkBuilder::with_strings(strings),
            scope: ScopeTracker::new(),
            errors: Vec::new(),
            symbols: ChunkSymbols::new(),
        };
        
        // create main chunk
        let info = ChunkInfo {
            symbol: None,
        };
        
        let chunk_id = compiler.new_chunk(info).unwrap();
        debug_assert!(chunk_id == CHUNK_MAIN);
        
        compiler
    }
    
    fn new_chunk(&mut self, info: ChunkInfo) -> CompileResult<ChunkID> {
        let chunk_id = self.builder.new_chunk(info)?;
        if !self.symbols.contains_key(&chunk_id) {
            self.symbols.insert(chunk_id, DebugSymbolsRLE::new());
        }
        Ok(chunk_id)
    }
    
    fn get_chunk(&mut self, chunk_id: ChunkID) -> CodeGenerator {
        CodeGenerator {
            compiler: self,
            chunk_id,
        }
    }
    
    fn main_chunk(&mut self) -> CodeGenerator {
        self.get_chunk(CHUNK_MAIN)
    }
    
    pub fn compile_program<'a>(mut self, program: impl Iterator<Item=&'a StmtMeta>) -> Result<CompiledProgram, Vec<CompileError>> {
        for stmt in program {
            self.push_stmt(stmt);
        }
        self.finish()
    }
    
    pub fn push_stmt(&mut self, stmt: &StmtMeta) {
        if let Err(error) = self.main_chunk().push_stmt(stmt) {
            self.errors.push(error);
        }
    }
    
    pub fn finish(mut self) -> Result<CompiledProgram, Vec<CompileError>> {
        if self.errors.is_empty() {
            self.main_chunk().finish();
            
            let output = CompiledProgram {
                program: self.builder.build(),
                symbols: self.symbols,
            };
            
            Ok(output)
        } else {
            Err(self.errors)
        }
    }

}

struct CodeGenerator<'c> {
    compiler: &'c mut Compiler,
    chunk_id: ChunkID,
}

impl CodeGenerator<'_> {
    
    pub fn push_stmt(&mut self, stmt: &StmtMeta) -> CompileResult<()> {
        let symbol = stmt.debug_symbol();
        
        let result = self.compile_stmt(Some(symbol), stmt.variant());
        if let Err(error) = result {
            Err(error.with_symbol(*symbol))
        } else {
            Ok(())
        }
    }
    
    pub fn finish(mut self) {
        self.emit_instr(None, OpCode::Return);
    }
    
    fn chunk_id(&self) -> ChunkID { self.chunk_id }
    
    fn builder(&self) -> &ChunkBuilder { &self.compiler.builder }
    fn builder_mut(&mut self) -> &mut ChunkBuilder { &mut self.compiler.builder }
    
    fn scope(&self) -> &ScopeTracker { &self.compiler.scope }
    fn scope_mut(&mut self) -> &mut ScopeTracker { &mut self.compiler.scope }
    
    fn symbols(&self) -> &ChunkSymbols { &self.compiler.symbols }
    fn symbols_mut(&mut self) -> &mut ChunkSymbols { &mut self.compiler.symbols }
    
    fn chunk(&self) -> &ChunkBuf {
        self.builder().chunk(self.chunk_id)
    }
    
    fn chunk_mut(&mut self) -> &mut ChunkBuf {
        self.builder_mut().chunk_mut(0)
    }
    
    fn current_offset(&self) -> usize {
        self.chunk().len()
    }
    
    fn push_symbol(&mut self, symbol: Option<&DebugSymbol>) {
        let chunk_id = self.chunk_id;
        self.symbols_mut()
            .get_mut(&chunk_id).unwrap()
            .push(symbol.map(|symbol| *symbol))
    }
    
    fn create_chunk(&mut self, symbol: Option<&DebugSymbol>) -> CompileResult<CodeGenerator> {
        let info = ChunkInfo {
            symbol: symbol.map(|symbol| *symbol),
        };
        let chunk_id = self.compiler.new_chunk(info)?;
        Ok(self.compiler.get_chunk(chunk_id))
    }
    
    ///////// Emitting Bytecode /////////
    
    fn emit_instr(&mut self, symbol: Option<&DebugSymbol>, opcode: OpCode) {
        debug_assert!(opcode.instr_len() == 1);
        self.push_symbol(symbol);
        self.chunk_mut().push_byte(opcode);
    }
    
    fn emit_instr_byte(&mut self, symbol: Option<&DebugSymbol>, opcode: OpCode, byte: u8) {
        debug_assert!(opcode.instr_len() == 2);
        self.push_symbol(symbol);
        self.chunk_mut().push_byte(opcode);
        self.chunk_mut().push_byte(byte);
    }
    
    fn emit_instr_data(&mut self, symbol: Option<&DebugSymbol>, opcode: OpCode, bytes: &[u8]) {
        debug_assert!(opcode.instr_len() == 1 + bytes.len());
        self.push_symbol(symbol);
        self.chunk_mut().push_byte(opcode);
        self.chunk_mut().extend_bytes(&bytes);
    }
    
    ///////// Patching Bytecode /////////
    
    fn patch_instr_data<const N: usize>(&mut self, offset: usize, opcode: OpCode, bytes: &[u8; N]) {
        debug_assert!(opcode.instr_len() == 1 + N);
        
        self.chunk_mut().as_mut_slice()[offset] = u8::from(opcode);
        self.chunk_mut().patch_bytes(offset + 1, bytes);
    }
    
    fn emit_dummy_instr(&mut self, symbol: Option<&DebugSymbol>, width: usize) {
        self.push_symbol(symbol);
        
        for i in 0..width {
            self.chunk_mut().push_byte(OpCode::Nop);
        }
    }
    
    ///////// Constants /////////
    
    fn get_or_make_const(&mut self, symbol: Option<&DebugSymbol>, value: Constant) -> CompileResult<ConstID> {
        self.builder_mut().get_or_insert_const(value)
    }
    
    fn emit_load_const(&mut self, symbol: Option<&DebugSymbol>, value: Constant) -> CompileResult<()> {
        let cid = self.get_or_make_const(symbol, value)?;
        
        if cid <= u8::MAX.into() {
            self.emit_instr_byte(symbol, OpCode::LoadConst, u8::try_from(cid).unwrap());
        } else {
            self.emit_instr_data(symbol, OpCode::LoadConst16, &cid.to_le_bytes());
        }
        Ok(())
    }
    
    ///////// Jumps /////////
    
    fn emit_jump_instr(&mut self, symbol: Option<&DebugSymbol>, jump: Jump, target: usize) -> CompileResult<()> {
        let jump_site = self.current_offset();
        let guess_width = jump.dummy_width();  // guess the width of the jump instruction
        
        let mut jump_offset = Self::calc_jump_offset(jump_site + guess_width, target)?;
        let mut jump_opcode = get_jump_opcode(jump, jump_offset);
        
        if guess_width != jump_opcode.instr_len() {
            // guessed wrong, need to recalc offset with new width
            let new_width = jump_opcode.instr_len();
            let new_offset = Self::calc_jump_offset(jump_site + new_width, target)?;
            let new_opcode = get_jump_opcode(jump, new_offset);
            
            // if we *still* don't have the right width, just abort
            if new_width != new_opcode.instr_len() {
                return Err(ErrorKind::CalcJumpOffsetFailed.into());
            }
            
            jump_offset = new_offset;
            jump_opcode = new_opcode;
        }
        
        match jump_offset {
            JumpOffset::Short(offset) => self.emit_instr_data(symbol, jump_opcode, &offset.to_le_bytes()),
            JumpOffset::Long(offset)  => self.emit_instr_data(symbol, jump_opcode, &offset.to_le_bytes()),
        }
        Ok(())
    }
    
    fn patch_jump_instr(&mut self, jump: Jump, jump_site: usize, dummy_width: usize, target: usize) -> CompileResult<()> {
        let mut jump_offset = Self::calc_jump_offset(jump_site + dummy_width, target)?;
        let mut jump_opcode = get_jump_opcode(jump, jump_offset);
        
        if dummy_width != jump_opcode.instr_len() {
            // need to recalculate offset with the new width
            let new_width = jump_opcode.instr_len();
            let new_offset = Self::calc_jump_offset(jump_site + new_width, target)?;
            let new_opcode = get_jump_opcode(jump, new_offset);
            
            // if we *still* don't have the right width, just abort
            if new_width != new_opcode.instr_len() {
                return Err(ErrorKind::CalcJumpOffsetFailed.into());
            }
            
            jump_offset = new_offset;
            jump_opcode = new_opcode;
            self.chunk_mut().resize_patch(jump_site, dummy_width, new_width);
        }
        
        match jump_offset {
            JumpOffset::Short(offset) => self.patch_instr_data(jump_site, jump_opcode, &offset.to_le_bytes()),
            JumpOffset::Long(offset)  => self.patch_instr_data(jump_site, jump_opcode, &offset.to_le_bytes()),
        }
        Ok(())
    }
    
    // Expects the *end* offset of the jump instruction
    fn calc_jump_offset(jump_end_offset: usize, target: usize) -> CompileResult<JumpOffset> {
        // inefficent, but this is compile time so that's okay
        let target = i128::try_from(target).unwrap();
        let jump_site = i128::try_from(jump_end_offset).unwrap();
        
        if let Ok(offset) = i16::try_from(target - jump_site) {
            return Ok(JumpOffset::Short(offset));
        }
        
        if let Ok(offset) = i32::try_from(target - jump_site) {
            return Ok(JumpOffset::Long(offset));
        }
        
        Err(ErrorKind::CalcJumpOffsetFailed.into())
    }
    
    ///////// Scopes /////////
    
    fn emit_begin_scope(&mut self, tag: ScopeTag, symbol: Option<&DebugSymbol>) {
        self.scope_mut().push_scope(tag, symbol);
    }
    
    fn emit_end_scope(&mut self) {
        let scope = self.scope_mut().pop_scope();
        let symbol = scope.debug_symbol();
        
        // discard all the locals from the stack
        let mut discard = scope.locals.len();
        while discard > u8::MAX.into() {
            self.emit_instr_byte(symbol, OpCode::DropLocals, u8::MAX);
            discard -= usize::from(u8::MAX);
        }
        
        if discard > 0 {
            self.emit_instr_byte(symbol, OpCode::DropLocals, u8::try_from(discard).unwrap());
        }
    }
    
    ///////// Statements /////////
    
    fn compile_stmt(&mut self, symbol: Option<&DebugSymbol>, stmt: &Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Loop { label, body } => self.compile_loop(symbol, label.as_ref(), body)?,
            
            Stmt::WhileLoop { label, condition, body } => self.compile_while_loop(symbol, label.as_ref(), condition, body)?,
            
            Stmt::ForLoop { } => unimplemented!(),
            
            Stmt::Echo(expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_instr(symbol, OpCode::Inspect);
                self.emit_instr(symbol, OpCode::Pop);
            },
            Stmt::Assert(expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_instr(symbol, OpCode::Assert);
                self.emit_instr(symbol, OpCode::Pop);
            }
            
            Stmt::Expression(expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_instr(symbol, OpCode::Pop);
            },
        }
        Ok(())
    }
    
    fn compile_loop(&mut self, symbol: Option<&DebugSymbol>, label: Option<&Label>, body: &StmtList) -> CompileResult<()> {
        
        let loop_target = self.current_offset();
        
        self.emit_begin_scope(ScopeTag::Loop, symbol);
        self.compile_stmt_list(body)?;
        self.emit_end_scope();
        
        self.emit_jump_instr(symbol, Jump::Uncond, loop_target)?;
        
        Ok(())
    }
    
    fn compile_while_loop(&mut self, symbol: Option<&DebugSymbol>, label: Option<&Label>, condition: &Expr, body: &StmtList) -> CompileResult<()> {
        
        // first iteration conditional jump
        self.compile_expr(symbol, condition)?;
        
        let end_jump_site = self.current_offset();
        self.emit_dummy_instr(symbol, Jump::PopIfFalse.dummy_width());
        
        let loop_target = self.current_offset();
        
        self.emit_begin_scope(ScopeTag::Loop, symbol);
        self.compile_stmt_list(body)?;
        self.emit_end_scope();
        
        // rest iteration conditional jump
        self.compile_expr(symbol, condition)?;
        self.emit_jump_instr(symbol, Jump::PopIfTrue, loop_target)?;
        
        let end_jump_target = self.current_offset();
        self.patch_jump_instr(Jump::PopIfFalse, end_jump_site, Jump::PopIfFalse.dummy_width(), end_jump_target)?;
        
        Ok(())
    }
    
    ///////// Expressions /////////
    
    fn compile_expr(&mut self, symbol: Option<&DebugSymbol>, expr: &Expr) -> CompileResult<()> {
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
            
            Expr::Block { label, suite } => self.compile_block(symbol, label.as_ref(), suite)?,
            Expr::IfExpr { branches, else_clause } => self.compile_if_expr(symbol, branches, else_clause.as_ref().map(|expr| &**expr))?,
            
            Expr::FunctionDef(fundef) => self.compile_function_def(symbol, fundef)?,
        }
        Ok(())
    }
    
    fn compile_block(&mut self, symbol: Option<&DebugSymbol>, label: Option<&Label>, suite: &ExprBlock) -> CompileResult<()> {
        
        self.emit_begin_scope(ScopeTag::Block, symbol);
        self.compile_expr_block(symbol, suite)?;
        self.emit_end_scope();
        
        Ok(())
    }
    
    fn compile_if_expr(&mut self, symbol: Option<&DebugSymbol>, branches: &[ConditionalBranch], else_clause: Option<&ExprBlock>) -> CompileResult<()> {
        debug_assert!(!branches.is_empty());
        
        // track the sites where we jump to the end, so we can patch them later
        let mut end_jump_sites = Vec::new();
        
        // if there is no else branch, the last non-else branch won't have a jump to end, and won't pop the condition 
        let (last_branch, rest) = branches.split_last().unwrap();
        let iter_branches = rest.iter()
            .map(|branch| (false, branch))
            .chain(iter::once((true, last_branch)));
        
        for (is_last, branch) in iter_branches {
            let is_final_branch = is_last && else_clause.is_none();
            
            self.compile_expr(symbol, branch.condition())?;
            
            let branch_jump_site = self.current_offset();
            let branch_jump = 
                if is_final_branch { Jump::IfFalse } // keep condition value
                else { Jump::PopIfFalse };
            self.emit_dummy_instr(symbol, branch_jump.dummy_width());
            
            self.emit_begin_scope(ScopeTag::Branch, symbol);
            self.compile_expr_block(symbol, branch.suite())?;
            self.emit_end_scope();
            
            // site for the jump to the end of if-expression
            if !is_final_branch {
                let end_offset = self.current_offset();
                end_jump_sites.push(end_offset);
                self.emit_dummy_instr(symbol, Jump::Uncond.dummy_width());
            }
            
            // target for the jump from the conditional of the now compiled branch
            let branch_target = self.current_offset();
            self.patch_jump_instr(branch_jump, branch_jump_site, branch_jump.dummy_width(), branch_target)?;
        }
        
        // else clause
        if let Some(suite) = else_clause {
            
            self.emit_begin_scope(ScopeTag::Branch, symbol);
            self.compile_expr_block(symbol, suite)?;
            self.emit_end_scope();
            
        }
        
        // patch all of the end jump sites
        let end_target = self.current_offset();
        for jump_site in end_jump_sites.iter() {
            self.patch_jump_instr(Jump::Uncond, *jump_site, Jump::Uncond.dummy_width(), end_target)?;
        }
        
        Ok(())
    }
    
    fn compile_function_def(&mut self, symbol: Option<&DebugSymbol>, fundef: &FunctionDef) -> CompileResult<()> {
        // compile function body first
        let mut codegen = self.create_chunk(symbol)?;
        let chunk_id = codegen.chunk_id();
        
        for stmt in fundef.body().iter() {
            codegen.push_stmt(stmt);
        }
        codegen.finish();
        
        let signature = self.compile_function_signature(symbol, fundef.signature())?;
        let function_id = self.builder_mut().push_function(signature);
        
        self.emit_load_const(symbol, Constant::Function(chunk_id, function_id))
    }
    
    fn compile_function_signature(&mut self, symbol: Option<&DebugSymbol>, sigdef: &SignatureDef) -> CompileResult<Signature> {
        let mut required = Vec::new();
        for paramdef in sigdef.required.iter() {
            let name = self.get_or_make_const(symbol, Constant::from(paramdef.name))?;
            let param = Parameter::new(name, paramdef.decl, None);
            required.push(param);
        }
        
        let mut default = Vec::new();
        for paramdef in sigdef.default.iter() {
            let name = self.get_or_make_const(symbol, Constant::from(paramdef.name))?;
            
            // compile default value expression
            let inner_symbol = Some(paramdef.default.debug_symbol());
            let default_expr = paramdef.default.variant();
            let mut codegen = self.create_chunk(inner_symbol)?;
            let chunk_id = codegen.chunk_id();
            
            codegen.compile_expr(inner_symbol, default_expr)?;
            codegen.finish();
            
            let param = Parameter::new(name, paramdef.decl, Some(chunk_id));
            default.push(param);
        }
        
        let mut variadic = None;
        if let Some(paramdef) = &sigdef.variadic {
            let name = self.get_or_make_const(symbol, Constant::from(paramdef.name))?;
            
            // compile default value expression
            let mut default_value = None;
            if let Some(expr) = &paramdef.default {
                let inner_symbol = Some(expr.debug_symbol());
                let mut codegen = self.create_chunk(inner_symbol)?;
                default_value.replace(codegen.chunk_id());
                
                codegen.compile_expr(inner_symbol, expr.variant())?;
                codegen.finish();
            }
            
            let param = Parameter::new(name, paramdef.decl, default_value);
            variadic.replace(param);
        }
        
        Ok(Signature::new(required, default, variadic))
    }
    
    fn compile_stmt_list(&mut self, stmt_list: &StmtList) -> CompileResult<()> {
        // compile stmt suite
        for stmt in stmt_list.iter() {
            let inner_symbol = stmt.debug_symbol();
            self.compile_stmt(Some(inner_symbol), stmt.variant())
                .map_err(|err| err.with_symbol(*inner_symbol))?;
        }
        
        // handle control flow
        if let Some(control) = stmt_list.end_control() {
            match control {
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
        
        Ok(())
    }
    
    fn compile_expr_block(&mut self, symbol: Option<&DebugSymbol>, suite: &ExprBlock) -> CompileResult<()> {
        self.compile_stmt_list(suite.stmt_list())?;
        
        // result expression
        if let Some(expr) = suite.result() {
            let inner_symbol = expr.debug_symbol();
            self.compile_expr(Some(inner_symbol), expr.variant())
                .map_err(|err| err.with_symbol(*inner_symbol))?;
        } else {
            self.emit_instr(symbol, OpCode::Nil); // implicit nil
        }
        
        Ok(())
    }
    
    fn compile_declaration(&mut self, symbol: Option<&DebugSymbol>, decl: DeclarationRef) -> CompileResult<()> {
        match &decl.lhs {
            LValue::Identifier(name) => if self.scope().is_global_scope() {
                self.compile_decl_global_name(symbol, decl.decl, *name, &decl.init)
            } else {
                self.compile_decl_local_name(symbol, decl.decl, *name, &decl.init)
            },
            
            LValue::Attribute(target) => unimplemented!(),
            LValue::Index(target) => unimplemented!(),
            
            LValue::Tuple(target_list) => match &decl.init {
                
                Expr::Tuple(init_list) => {
                    if target_list.len() != init_list.len() {
                        return Err(CompileError::new("can't assign tuples of different lengths"))
                    }
                    
                    for (inner_lhs, inner_expr) in target_list.iter().zip(init_list.iter()) {
                        let inner_symbol = inner_expr.debug_symbol();
                        let inner_init = inner_expr.variant();
                        
                        let inner_decl = DeclarationRef {
                            decl: decl.decl,
                            lhs: inner_lhs,
                            init: inner_init,
                        };
                        
                        self.compile_declaration(Some(inner_symbol), inner_decl)
                            .map_err(|err| err.with_symbol(*inner_symbol))?;
                    }
                    
                    // declarations are also expressions
                    let tuple_len = u8::try_from(init_list.len())
                        .map_err(|_| CompileError::from(ErrorKind::TupleLengthLimit))?;
                    self.emit_instr_byte(symbol, OpCode::Tuple, tuple_len);
                    
                    Ok(())
                },
                
                _ => unimplemented!(), // dynamic declaration
            },
        }
    }
    
    fn compile_decl_local_name(&mut self, symbol: Option<&DebugSymbol>, decl: DeclType, name: InternSymbol, init: &Expr) -> CompileResult<()> {
        
        self.compile_expr(symbol, init)?;  // make sure to evaluate initializer first in order for shadowing to work
        
        self.scope_mut().insert_local(decl, name)?;
        self.emit_instr(symbol, OpCode::InsertLocal);
        Ok(())
    }
    
    fn compile_decl_global_name(&mut self, symbol: Option<&DebugSymbol>, decl: DeclType, name: InternSymbol, init: &Expr) -> CompileResult<()> {
        
        self.compile_expr(symbol, init)?;
        
        self.emit_load_const(symbol, Constant::from(name))?;
        match decl {
            DeclType::Immutable => self.emit_instr(symbol, OpCode::InsertGlobal),
            DeclType::Mutable => self.emit_instr(symbol, OpCode::InsertGlobalMut),
        }
        Ok(())
    }
    
    fn compile_assignment(&mut self, symbol: Option<&DebugSymbol>, assign: AssignmentRef) -> CompileResult<()> {
        match assign.lhs {
            // LValue::Identifier(name) if self.state.is_global_scope() => self.compile_assign_global_name(symbol, op, *name, &rhs),
            LValue::Identifier(name) => self.compile_assign_identifier(symbol, name, assign),
            
            LValue::Attribute(target) => unimplemented!(),
            LValue::Index(target) => unimplemented!(),
            
            LValue::Tuple(..) if assign.op.is_some() => {
                return Err(CompileError::new("can't use update-assigment when assigning to a tuple"))
            },
            
            LValue::Tuple(target_list) => match assign.rhs {

                Expr::Tuple(rhs_list) => {
                    if target_list.len() != rhs_list.len() {
                        return Err(CompileError::new("can't assign tuples of different lengths"))
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
                        
                        self.compile_assignment(Some(inner_symbol), inner_assign)
                            .map_err(|err| err.with_symbol(*inner_symbol))?;
                    }
                    
                    // assignments are also expressions
                    let tuple_len = u8::try_from(rhs_list.len())
                        .map_err(|_| CompileError::from(ErrorKind::TupleLengthLimit))?;
                    self.emit_instr_byte(symbol, OpCode::Tuple, tuple_len);
                    
                    Ok(())
                },
                
                _ => unimplemented!(), // dynamic declaration

            },
        }
    }
    
    fn compile_assign_identifier(&mut self, symbol: Option<&DebugSymbol>, name: &InternSymbol, assign: AssignmentRef) -> CompileResult<()> {
        
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
        
        if self.scope().local_scope().is_some() {
            
            // check if the name is found in the local scope...
            let result = self.scope().resolve_local_strict(name).map(|local| (local.decl, local.offset));
            
            if let Some((decl, offset)) = result {
                if decl != DeclType::Mutable {
                    return Err(CompileError::from(ErrorKind::CantAssignImmutable));
                }
                self.emit_assign_local(symbol, offset);
                
                return Ok(());
            }
            
            // otherwise search enclosing scopes...
            
            let result = self.scope().resolve_nonlocal(name).map(|local| (local.decl, local.offset));
            
            if let Some((decl, offset)) = result {
                if !assign.nonlocal {
                    return Err(CompileError::from(ErrorKind::CantAssignNonLocal));
                }
                if decl != DeclType::Mutable {
                    return Err(CompileError::from(ErrorKind::CantAssignImmutable));
                }
                self.emit_assign_local(symbol, offset);
                
                return Ok(());
            }
            
        }
        
        // ...finally, try to assign global
        
        // allow assignment to global only if all enclosing scopes permit it
        if !assign.nonlocal && !self.scope().iter_scopes().all(|scope| scope.allow_enclosing_assignment()) {
            return Err(CompileError::from(ErrorKind::CantAssignNonLocal));
        }
        
        self.emit_load_const(symbol, Constant::from(*name))?;
        self.emit_instr(symbol, OpCode::StoreGlobal);
        Ok(())
    }
    
    fn emit_assign_local(&mut self, symbol: Option<&DebugSymbol>, offset: Offset) {
        if let Ok(offset) = u8::try_from(offset) {
            self.emit_instr_byte(symbol, OpCode::StoreLocal, offset);
        } else {
            self.emit_instr_data(symbol, OpCode::StoreLocal16, &offset.to_le_bytes());
        }
    }
    
    fn compile_tuple(&mut self, symbol: Option<&DebugSymbol>, expr_list: &[ExprMeta]) -> CompileResult<()> {
        let len = u8::try_from(expr_list.len())
            .map_err(|_| CompileError::from(ErrorKind::TupleLengthLimit))?;
        
        for expr in expr_list.iter() {
            let inner_symbol = expr.debug_symbol();
            self.compile_expr(Some(inner_symbol), expr.variant())
                .map_err(|err| err.with_symbol(*inner_symbol))?;
        }
        
        self.emit_instr_byte(symbol, OpCode::Tuple, len);
        Ok(())
    }
    
    fn compile_atom(&mut self, symbol: Option<&DebugSymbol>, atom: &Atom) -> CompileResult<()> {
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
    
    fn compile_name_lookup(&mut self, symbol: Option<&DebugSymbol>, name: &InternSymbol) -> CompileResult<()> {
        
        let local_offset = self.scope().resolve_local(name).map(|local| local.offset);
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
    
    fn compile_primary(&mut self, symbol: Option<&DebugSymbol>, primary: &Primary) -> CompileResult<()> {
        unimplemented!()
    }
    
    fn emit_unary_op(&mut self, symbol: Option<&DebugSymbol>, op: &UnaryOp) {
        match op {
            UnaryOp::Neg => self.emit_instr(symbol, OpCode::Neg),
            UnaryOp::Pos => self.emit_instr(symbol, OpCode::Pos),
            UnaryOp::Inv => self.emit_instr(symbol, OpCode::Inv),
            UnaryOp::Not => self.emit_instr(symbol, OpCode::Not),
        }
    }
    
    fn emit_binary_op(&mut self, symbol: Option<&DebugSymbol>, op: &BinaryOp) {
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
