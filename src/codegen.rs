use core::iter;

use crate::language::{IntType, FloatType, InternSymbol, Access};
use crate::parser::stmt::{StmtMeta, Stmt, Label, StmtList, ControlFlow};
use crate::parser::expr::{Expr, ExprMeta, ExprBlock, ConditionalBranch};
use crate::parser::primary::{Atom, Primary, AccessItem};
use crate::parser::pattern::{Pattern, MatchAction};
use crate::parser::fundefs::{FunctionDef, SignatureDef};
use crate::parser::operator::{UnaryOp, BinaryOp};
use crate::runtime::strings::{StringInterner};
use crate::runtime::errors::ErrorKind;
use crate::debug::symbol::{DebugSymbol, ChunkSymbols, DebugSymbolTable};

mod scope;

pub mod chunk;
pub mod consts;
pub mod funproto;
pub mod opcodes;
pub mod errors;

pub use opcodes::{OpCode, LocalIndex};
pub use chunk::{UnloadedProgram, Program, ProgramData, Chunk};
pub use consts::{ConstID, Constant};
pub use funproto::{FunctionID, FunctionProto, UpvalueTarget};
pub use errors::{CompileResult, CompileError};

use scope::{ScopeTracker, ScopeTag, Scope, LocalName, InsertLocal, ControlFlowTarget};
use chunk::{ChunkBuilder, ChunkInfo, ChunkBuf};
use funproto::{UnloadedFunction, UnloadedSignature, UnloadedParam};


// Helpers

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum JumpOffset {
    Short(i16),
    Long(i32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
        
        (Jump::Uncond,  JumpOffset::Long(..))  => OpCode::LongJump,
        (Jump::IfFalse, JumpOffset::Long(..))  => OpCode::LongJumpIfFalse,
        (Jump::IfTrue,  JumpOffset::Long(..))  => OpCode::LongJumpIfTrue,
        
        (Jump::PopIfFalse, JumpOffset::Short(..))  => OpCode::PopJumpIfFalse,
        (Jump::PopIfTrue,  JumpOffset::Short(..))  => OpCode::PopJumpIfTrue,
        
        (Jump::PopIfFalse, JumpOffset::Long(..))   => OpCode::PopLongJumpIfFalse,
        (Jump::PopIfTrue,  JumpOffset::Long(..))   => OpCode::PopLongJumpIfTrue,
    }
}

// represents the site of a dummy jump instruction that will be patched with a target later
#[derive(Debug)]
struct JumpSite {
    jump: Jump,
    offset: usize,
    width: usize,
}


/// Output container
#[derive(Debug)]
pub struct CompiledProgram {
    pub program: UnloadedProgram,
    pub symbols: ChunkSymbols,
}


// Code Generator
pub struct Compiler {
    builder: ChunkBuilder,
    scopes: ScopeTracker,
    errors: Vec<CompileError>,
    symbols: ChunkSymbols,
}

impl Compiler {
    pub fn new(strings: StringInterner) -> Self {
        // insert symbol container for main chunk
        let mut symbols = ChunkSymbols::new();
        symbols.insert(Chunk::Main, DebugSymbolTable::new());
        
        Self {
            builder: ChunkBuilder::with_strings(strings),
            scopes: ScopeTracker::new(),
            errors: Vec::new(),
            symbols,
        }
    }
    
    fn new_chunk(&mut self, info: ChunkInfo) -> CompileResult<Chunk> {
        let chunk_id = self.builder.new_chunk(info)?;
        self.symbols.entry(chunk_id)
            .or_insert_with(DebugSymbolTable::new);
        
        Ok(chunk_id)
    }
    
    fn get_chunk(&mut self, chunk_id: Chunk) -> CodeGenerator {
        CodeGenerator::new(self, chunk_id)
    }
    
    pub fn compile_program<'a>(mut self, program: impl Iterator<Item=&'a StmtMeta>) -> Result<CompiledProgram, Vec<CompileError>> {
        for stmt in program {
            self.push_stmt(stmt);
        }
        self.finish()
    }
    
    pub fn push_stmt(&mut self, stmt: &StmtMeta) {
        if let Err(error) = self.get_chunk(Chunk::Main).push_stmt(stmt) {
            self.errors.push(error);
        }
    }
    
    pub fn finish(mut self) -> Result<CompiledProgram, Vec<CompileError>> {
        if self.errors.is_empty() {
            self.get_chunk(Chunk::Main)
                .finish();
            
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
    chunk_id: Chunk,
    symbols: Vec<Option<DebugSymbol>>,
}

impl<'c> CodeGenerator<'c> {
    fn new(compiler: &'c mut Compiler, chunk_id: Chunk) -> Self {
        Self {
            compiler, chunk_id,
            symbols: Vec::new(),
        }
    }
}

impl CodeGenerator<'_> {
    
    pub fn push_stmt(&mut self, stmt: &StmtMeta) -> CompileResult<()> {
        let symbol = stmt.debug_symbol();
        self.push_symbol(Some(*symbol));
        
        let result = self.compile_stmt(stmt.variant());
        if let Err(mut error) = result {
            let symbol = self.symbols.iter().rev()
                .find_map(|s| s.as_ref());
            if let Some(symbol) = symbol {
                error = error.with_symbol(*symbol);
            }
            
            Err(error)
        } else {
            self.pop_symbol();
            Ok(())
        }
    }
    
    pub fn finish(mut self) {
        self.symbols.clear();
        match self.chunk_id {
            Chunk::Main => self.emit_instr(OpCode::Exit),
            Chunk::Function(..) => self.emit_instr(OpCode::Return),
        }
    }
    
    fn chunk_id(&self) -> Chunk { self.chunk_id }
    
    fn builder(&self) -> &ChunkBuilder { &self.compiler.builder }
    fn builder_mut(&mut self) -> &mut ChunkBuilder { &mut self.compiler.builder }
    
    fn scopes(&self) -> &ScopeTracker { &self.compiler.scopes }
    fn scopes_mut(&mut self) -> &mut ScopeTracker { &mut self.compiler.scopes }
    
    fn symbols(&self) -> &ChunkSymbols { &self.compiler.symbols }
    fn symbols_mut(&mut self) -> &mut ChunkSymbols { &mut self.compiler.symbols }
    
    fn chunk(&self) -> &ChunkBuf {
        self.builder().chunk(self.chunk_id)
    }
    
    fn chunk_mut(&mut self) -> &mut ChunkBuf {
        let chunk_id = self.chunk_id;
        self.builder_mut().chunk_mut(chunk_id)
    }
    
    fn current_offset(&self) -> usize {
        self.chunk().len()
    }
    
    fn current_symbol(&self) -> Option<DebugSymbol> {
        self.symbols.last().copied().flatten()
    }
    
    fn push_symbol(&mut self, symbol: Option<DebugSymbol>) {
        self.symbols.push(symbol)
    }
    
    fn pop_symbol(&mut self) -> Option<DebugSymbol> {
        self.symbols.pop().flatten()
    }
    
    fn emit_symbol(&mut self, symbol: DebugSymbol) {
        let chunk_id = self.chunk_id;
        let offset = self.current_offset();
        self.symbols_mut()
            .get_mut(&chunk_id).unwrap()
            .insert(offset, symbol)
    }
    
    fn create_chunk(&mut self, metadata: ChunkInfo) -> CompileResult<CodeGenerator> {
        let chunk_id = self.compiler.new_chunk(metadata)?;
        Ok(self.compiler.get_chunk(chunk_id))
    }
}


///////// Emitting Bytecode /////////
impl CodeGenerator<'_> {
    fn emit_instr(&mut self, opcode: OpCode) {
        debug_assert!(opcode.instr_len() == 1);
        
        if let Some(symbol) = self.current_symbol() {
            self.emit_symbol(symbol);
        }
        
        self.chunk_mut().push_byte(opcode);
    }
    
    fn emit_instr_byte(&mut self, opcode: OpCode, byte: u8) {
        debug_assert!(opcode.instr_len() == 2);
        
        if let Some(symbol) = self.current_symbol() {
            self.emit_symbol(symbol);
        }
        
        self.chunk_mut().push_byte(opcode);
        self.chunk_mut().push_byte(byte);
    }
    
    fn emit_instr_data(&mut self, opcode: OpCode, bytes: &[u8]) {
        debug_assert!(opcode.instr_len() == 1 + bytes.len());
        
        if let Some(symbol) = self.current_symbol() {
            self.emit_symbol(symbol);
        }
        
        self.chunk_mut().push_byte(opcode);
        self.chunk_mut().extend_bytes(bytes);
    }
}

///////// Patching Bytecode /////////
impl CodeGenerator<'_> {
    fn patch_instr_data<const N: usize>(&mut self, offset: usize, opcode: OpCode, bytes: &[u8; N]) {
        debug_assert!(opcode.instr_len() == 1 + N);
        
        self.chunk_mut().as_mut_slice()[offset] = u8::from(opcode);
        self.chunk_mut().patch_bytes(offset + 1, bytes);
    }
    
    fn emit_dummy_instr(&mut self, width: usize) {
        if let Some(symbol) = self.current_symbol() {
            self.emit_symbol(symbol);
        }
        
        for _ in 0..width {
            self.chunk_mut().push_byte(OpCode::Nop);
        }
    }
}

///////// Constants /////////
impl CodeGenerator<'_> {
    fn get_or_make_const(&mut self, value: Constant) -> CompileResult<ConstID> {
        self.builder_mut().get_or_insert_const(value)
    }
    
    fn emit_load_const(&mut self, value: Constant) -> CompileResult<()> {
        let cid = self.get_or_make_const(value)?;
        
        if cid <= u8::MAX.into() {
            self.emit_instr_byte(OpCode::LoadConst, u8::try_from(cid).unwrap());
        } else {
            self.emit_instr_data(OpCode::LoadConst16, &cid.to_le_bytes());
        }
        Ok(())
    }
    
    fn emit_load_error(&mut self, error: ErrorKind, message: &str) -> CompileResult<()> {
        let cid = self.builder_mut().get_or_insert_error(error, message)?;
        
        if cid <= u8::MAX.into() {
            self.emit_instr_byte(OpCode::LoadConst, u8::try_from(cid).unwrap());
        } else {
            self.emit_instr_data(OpCode::LoadConst16, &cid.to_le_bytes());
        }
        Ok(())
    }
    
    fn make_function(&mut self, function: UnloadedFunction) {
        self.builder_mut().insert_function(function)
    }
    
    fn emit_load_function(&mut self, fun_id: FunctionID) {
        if fun_id <= u8::MAX.into() {
            self.emit_instr_byte(OpCode::LoadFunction, u8::try_from(fun_id).unwrap());
        } else {
            self.emit_instr_data(OpCode::LoadFunction16, &fun_id.to_le_bytes());
        }
    }
}

///////// Jumps /////////
impl CodeGenerator<'_> {
    fn emit_jump_instr(&mut self, jump: Jump, target: usize) -> CompileResult<()> {
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
                return Err("could not calculate jump offset".into());
            }
            
            jump_offset = new_offset;
            jump_opcode = new_opcode;
        }
        
        match jump_offset {
            JumpOffset::Short(offset) => self.emit_instr_data(jump_opcode, &offset.to_le_bytes()),
            JumpOffset::Long(offset)  => self.emit_instr_data(jump_opcode, &offset.to_le_bytes()),
        }
        Ok(())
    }
    
    fn emit_dummy_jump(&mut self, jump: Jump) -> JumpSite {
        let offset = self.current_offset();
        let jump_site = JumpSite {
            jump, offset,
            width: jump.dummy_width(),
        };
        
        self.emit_dummy_instr(jump_site.width);
        
        jump_site
    }
    
    fn patch_jump_instr(&mut self, jump: &JumpSite, target: usize) -> CompileResult<()> {
        let jump_type = jump.jump;
        let jump_site = jump.offset;
        let dummy_width = jump.width;
        
        let mut jump_offset = Self::calc_jump_offset(jump_site + dummy_width, target)?;
        let mut jump_opcode = get_jump_opcode(jump_type, jump_offset);
        
        if dummy_width != jump_opcode.instr_len() {
            // need to recalculate offset with the new width
            let new_width = jump_opcode.instr_len();
            let new_offset = Self::calc_jump_offset(jump_site + new_width, target)?;
            let new_opcode = get_jump_opcode(jump_type, new_offset);
            
            // if we *still* don't have the right width, just abort
            if new_width != new_opcode.instr_len() {
                return Err("could not calculate jump offset".into());
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
        
        Err("could not calculate jump offset".into())
    }
}

///////// Scopes /////////

// container for data needed to drop a scope
struct ScopeDrop {
    tag: ScopeTag,
    locals: usize,
    close_upvals: Vec<LocalIndex>,
}

impl From<&Scope> for ScopeDrop {
    fn from(scope: &Scope) -> Self {
        ScopeDrop {
            tag: scope.tag(),
            locals: scope.locals().len(),
            close_upvals: scope.locals().iter()
                .filter_map(|local| if local.captured() { Some(local.index()) } else { None })
                .collect(),
        }
    }
}

impl CodeGenerator<'_> {
    fn emit_begin_scope(&mut self, label: Option<&Label>, tag: ScopeTag) -> &mut Scope {
        let symbol = self.current_symbol();
        self.scopes_mut().push_scope(symbol.as_ref(), label.copied(), tag)
    }
    
    fn emit_end_scope(&mut self) -> Scope {
        let scope = self.scopes_mut().pop_scope();
        self.emit_scope_drop(&(&scope).into());
        scope
    }
    
    fn patch_break_sites(&mut self, scope: &Scope, break_target: usize) -> CompileResult<()> {
        for break_site in scope.break_sites().iter() {
            self.patch_jump_instr(break_site, break_target)?;
        }
        Ok(())
    }
    
    fn patch_continue_sites(&mut self, scope: &Scope, continue_target: usize) -> CompileResult<()> {
        for continue_site in scope.continue_sites().iter() {
            self.patch_jump_instr(continue_site, continue_target)?;
        }
        Ok(())
    }
    
    fn emit_scope_drop(&mut self, scope: &ScopeDrop) {
        // close all upvalues
        for local_index in scope.close_upvals.iter() {
            self.emit_close_upvalue(*local_index);
        }
        
        // discard all the locals from the stack
        let mut discard = scope.locals;
        while discard > u8::MAX.into() {
            self.emit_instr_byte(OpCode::DropLocals, u8::MAX);
            discard -= usize::from(u8::MAX);
        }
        
        if discard > 0 {
            self.emit_instr_byte(OpCode::DropLocals, u8::try_from(discard).unwrap());
        }
    }
    
    // If the local name cannot be found, no instructions are emitted and None is returned
    fn try_emit_load_local(&mut self, name: &LocalName) -> Option<u16> {
        if let Some(index) = self.scopes().resolve_local(name).map(|local| local.index()) {
            self.emit_load_local_index(index);
            Some(index)
        } else{
            None
        }
    }
    
    fn emit_load_local_index(&mut self, index: LocalIndex) {
        if let Ok(index) = u8::try_from(index) {
            self.emit_instr_byte(OpCode::LoadLocal, index);
        } else {
            self.emit_instr_data(OpCode::LoadLocal16, &index.to_le_bytes());
        }
    }
    
    fn try_emit_load_upval(&mut self, name: &LocalName) -> CompileResult<Option<u16>> {
        if let Some(index) = self.scopes_mut().resolve_or_create_upval(name)?.map(|upval| upval.index()) {
            if let Ok(index) = u8::try_from(index) {
                self.emit_instr_byte(OpCode::LoadUpvalue, index);
            } else {
                self.emit_instr_data(OpCode::LoadUpvalue16, &index.to_le_bytes());
            }
            
            Ok(Some(index))
        } else {
            Ok(None)
        }
    }
    
    fn emit_close_upvalue(&mut self, index: LocalIndex) {
        if let Ok(index) = u8::try_from(index) {
            self.emit_instr_byte(OpCode::CloseUpvalue, index);
        } else {
            self.emit_instr_data(OpCode::CloseUpvalue16, &index.to_le_bytes());
        }
    }
    
    fn emit_create_temporary(&mut self, access: Access) -> CompileResult<LocalIndex> {
        debug_assert!(self.scopes().is_temporary_scope());
        
        match self.scopes_mut().insert_local(access, LocalName::Anonymous)? {
            InsertLocal::CreateNew(local_index) => {
                self.emit_instr(OpCode::InsertLocal);
                Ok(local_index)
            }
            
            InsertLocal::HideExisting(local_index) => {
                self.emit_assign_local(local_index);
                Ok(local_index)
            }
        }
    }
}


///////// Statements /////////
impl CodeGenerator<'_> {
    fn compile_stmt_with_symbol(&mut self, stmt: &StmtMeta) -> CompileResult<()> {
        let symbol = stmt.debug_symbol();
        self.push_symbol(Some(*symbol));
        self.compile_stmt(stmt.variant())?;
        self.pop_symbol();
        Ok(())
    }
    
    fn compile_stmt(&mut self, stmt: &Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Loop { label, body } => self.compile_loop(label.as_ref(), body)?,
            
            Stmt::WhileLoop { label, condition, body } => self.compile_while_loop(label.as_ref(), condition, body)?,
            
            Stmt::ForLoop { label, pattern, iter, body } => self.compile_for_loop(label.as_ref(), pattern, iter, body)?,
            
            Stmt::Assert(expr) => {
                self.compile_expr(expr)?;
                self.emit_instr(OpCode::Assert);
                self.emit_instr(OpCode::Pop);
            }
            
            Stmt::Expression(expr) => {
                self.compile_expr(expr)?;
                self.emit_instr(OpCode::Pop);
            },
        }
        Ok(())
    }
    
    fn compile_stmt_list(&mut self, stmt_list: &StmtList) -> CompileResult<()> {
        // compile stmt suite
        for stmt in stmt_list.iter() {
            self.compile_stmt_with_symbol(stmt)?;
        }
        
        Ok(())
    }
    
    // compile a statment list that will not evaluate to a value
    fn compile_stmt_block(&mut self, stmt_list: &StmtList) -> CompileResult<()> {
        self.compile_stmt_list(stmt_list)?;
        if let Some(control) = stmt_list.end_control() {
            self.compile_control_flow(control)?;
        }
        Ok(())
    }
    
    fn compile_expr_block(&mut self, suite: &ExprBlock) -> CompileResult<()> {
        let stmt_list = suite.stmt_list();
        self.compile_stmt_list(stmt_list)?;
        
        if stmt_list.end_control().is_none() {
            // result expression
            if let Some(expr) = suite.result() {
                self.compile_expr_with_symbol(expr)?;
            } else {
                self.emit_instr(OpCode::Nil); // implicit nil
            }
        }
        
        if let Some(control) = stmt_list.end_control() {
            self.compile_control_flow(control)?;
        }
        
        Ok(())
    }
    
    fn compile_control_flow(&mut self, control_flow: &ControlFlow) -> CompileResult<()> {
        match control_flow {
            ControlFlow::Continue { label, symbol } => {
                self.push_symbol(*symbol);
                self.compile_continue_control(label.as_ref())?;
                self.pop_symbol();
            }
            
            ControlFlow::Break { label, expr, symbol } => {
                self.push_symbol(*symbol);
                self.compile_break_control(label.as_ref(), expr.as_deref())?;
                self.pop_symbol();
            }
            
            ControlFlow::Return { expr, symbol } => {
                self.push_symbol(*symbol);
                
                match expr {
                    Some(expr) => self.compile_expr(expr)?,
                    None => self.emit_instr(OpCode::Nil),
                }
                
                self.emit_instr(OpCode::Return);
                self.pop_symbol();
            }
        }
        Ok(())
    }
    
    fn compile_break_control(&mut self, label: Option<&Label>, expr: Option<&Expr>) -> CompileResult<()> {
        // find the target scope
        let target_depth = match self.scopes().resolve_control_flow(ControlFlowTarget::Break(label.copied())) {
            Some(scope) => scope.depth(),
            None => {
                let message =
                    if label.is_some() { "can't find loop or block with matching label for \"break\"" }
                    else { "\"break\" outside of loop or block" };
                return Err(message.into());
            },
        };
        
        // drop all scopes up to and including the target
        let scope_drop: Vec<ScopeDrop> = self.scopes().iter_scopes()
            .take_while(|scope| scope.depth() >= target_depth)
            .map(ScopeDrop::from)
            .collect();
        
        let (target, through_scopes) = scope_drop.split_last().unwrap();
        for scope in through_scopes.iter() {
            self.emit_scope_drop(scope);
            
            // expression blocks leave their value on the stack
            // (this is helped by the fact that break/contine must come last in a list of statements)
            // so if we break through an expression block we need to pop its value
            if scope.tag.is_expr_block() {
                self.emit_instr(OpCode::Pop);
            }
        }
        self.emit_scope_drop(target); // drop target scope
        
        // if breaking from an expression block, emit the expression value before jumping
        if target.tag.is_expr_block() {
            if let Some(expr) = expr {
                self.compile_expr(expr)?;
            } else {
                self.emit_instr(OpCode::Nil);
            }
        } else if expr.is_some() {
            return Err("\"break\" with value outside of block expression".into())
        }
        
        // emit jump site, register with scope
        let break_site = self.emit_dummy_jump(Jump::Uncond);
        
        let target_scope = self.scopes_mut().iter_scopes_mut()
            .find(|scope| scope.depth() == target_depth)
            .unwrap();
        target_scope.register_break(break_site);
        
        Ok(())
        
    }
    
    fn compile_continue_control(&mut self, label: Option<&Label>) -> CompileResult<()> {
        // find the target scope
        let target_depth = match self.scopes().resolve_control_flow(ControlFlowTarget::Continue(label.copied())) {
            Some(scope) => scope.depth(),
            None => {
                let message =
                    if label.is_some() { "can't find loop with matching label for \"continue\"" }
                    else { "\"continue\" outside of loop" };
                return Err(message.into());
            }
        };
        
        // drop all scopes up to and including the target
        let scope_drop: Vec<ScopeDrop> = self.scopes().iter_scopes()
            .take_while(|scope| scope.depth() >= target_depth)
            .map(ScopeDrop::from)
            .collect();
        
        for scope in scope_drop.iter() {
            self.emit_scope_drop(scope);
            
            // expression blocks leave their value on the stack
            // (this is helped by the fact that break/contine must come last in a list of statements)
            // so if we jump out of an expression block we need to pop its value
            if scope.tag.is_expr_block() {
                self.emit_instr(OpCode::Pop);
            }
        }
        
        // emit jump site, register with scope
        let continue_site = self.emit_dummy_jump(Jump::Uncond);
        
        let target_scope = self.scopes_mut().iter_scopes_mut()
            .find(|scope| scope.depth() == target_depth)
            .unwrap();
        target_scope.register_continue(continue_site);
        
        Ok(())
    }
    
    fn compile_loop(&mut self, label: Option<&Label>, body: &StmtList) -> CompileResult<()> {
        
        let loop_target = self.current_offset();
        
        self.emit_begin_scope(label, ScopeTag::Loop);
        
        self.compile_stmt_block(body)?;
        let loop_scope = self.emit_end_scope();
        
        self.emit_jump_instr(Jump::Uncond, loop_target)?;
        
        // finalize scope
        let break_target = self.current_offset();
        self.patch_break_sites(&loop_scope, break_target)?;
        self.patch_continue_sites(&loop_scope, loop_target)?;
        
        Ok(())
    }
    
    fn compile_while_loop(&mut self, label: Option<&Label>, condition: &Expr, body: &StmtList) -> CompileResult<()> {
        
        // first iteration conditional jump
        let continue_target = self.current_offset();
        self.compile_expr(condition)?;
        
        let end_jump_site = self.emit_dummy_jump(Jump::PopIfFalse);
        
        let loop_target = self.current_offset();
        
        self.emit_begin_scope(label, ScopeTag::Loop);
        self.compile_stmt_block(body)?;
        let loop_scope = self.emit_end_scope();
        
        // rest iteration conditional jump
        self.compile_expr(condition)?;
        self.emit_jump_instr(Jump::PopIfTrue, loop_target)?;
        
        self.patch_jump_instr(&end_jump_site, self.current_offset())?;
        
        // finalize scope
        let break_target = self.current_offset();
        self.patch_break_sites(&loop_scope, break_target)?;
        self.patch_continue_sites(&loop_scope, continue_target)?;
        
        Ok(())
    }
    
    fn compile_for_loop(&mut self, label: Option<&Label>, pattern: &Pattern, iter: &Expr, body: &StmtList) -> CompileResult<()> {
        
        self.emit_begin_scope(label, ScopeTag::Loop);
        
        // initialize iterator
        self.compile_expr(iter)?;
        self.emit_instr(OpCode::IterInit);
        
        // first iteration conditional jump
        let continue_target = self.current_offset();
        let end_jump_site = self.emit_dummy_jump(Jump::IfFalse);
        
        let loop_target = self.current_offset();
        
        // advance iterator and assign value
        // default to "let" for loop variables (unlike normal assignment, which defaults to "local")
        self.emit_instr(OpCode::IterNext);
        self.compile_assignment(MatchAction::DeclImmutable, pattern)?; 
        self.emit_instr(OpCode::Pop);
        
        // compile body
        self.compile_stmt_block(body)?;
        let loop_scope = self.emit_end_scope();
        
        // rest iteration conditional jump
        // should have just [ ... iter state[N] ] on the stack here
        self.emit_jump_instr(Jump::IfTrue, loop_target)?;
        
        let break_target = self.current_offset();
        self.emit_instr_byte(OpCode::Drop, 2); // drop [ iter state ]
        
        // finalize scope
        self.patch_jump_instr(&end_jump_site, break_target)?;
        self.patch_break_sites(&loop_scope, break_target)?;
        self.patch_continue_sites(&loop_scope, continue_target)?;
        
        Ok(())
    }
}

///////// Expressions /////////

// helper to indicate if the length of a sequence is statically known or if it is stored in a local
enum Unpack {
    Empty,
    Static(IntType),
    Dynamic,
}

impl CodeGenerator<'_> {
    fn compile_expr_with_symbol(&mut self, expr: &ExprMeta) -> CompileResult<()> {
        let symbol = expr.debug_symbol();
        self.push_symbol(Some(*symbol));
        self.compile_expr(expr.variant())?;
        self.pop_symbol();
        Ok(())
    }
    
    fn compile_expr(&mut self, expr: &Expr) -> CompileResult<()> {
        match expr {
            Expr::Atom(atom) => self.compile_atom(atom)?,
            
            Expr::Primary(primary) => self.compile_primary(primary)?,
            
            Expr::UnaryOp(op, expr) => self.compile_unary_op(*op, expr)?,
            
            Expr::BinaryOp(op, exprs) => {
                let (lhs, rhs) = &**exprs;
                self.compile_binary_op(*op, lhs, rhs)?;
            },
            
            Expr::Assignment(assign) => {
                if let Some(op) = assign.op {
                    self.compile_update_assignment(op, assign.action, &assign.lhs, &assign.rhs)?;
                } else {
                    self.compile_expr(&assign.rhs)?;
                    self.compile_assignment(assign.action, &assign.lhs)?;
                }
            },
            
            Expr::Tuple(items) => self.compile_tuple(items)?,
            
            Expr::Table(_fields) => unimplemented!(),
            
            // unpacking is only allowed in invocation, tuple literals, and by itself in parentheses
            // note: assignment uses *packing*, not unpacking, which is the Pattern dual of packing.
            Expr::Unpack(Some(..)) => return Err("unpack expression must be enclosed in parentheses".into()),
            Expr::Unpack(None) => return Err("\"...\" is not allowed here".into()),
            
            Expr::Block { label, suite } => self.compile_block_expression(label.as_ref(), suite)?,
            Expr::IfExpr { branches, else_clause } => self.compile_if_expression(branches, else_clause.as_ref().map(|expr| &**expr))?,
            
            Expr::FunctionDef(fundef) => self.compile_function_def(fundef)?,
        }
        Ok(())
    }
    
    fn compile_tuple(&mut self, expr_list: &[ExprMeta]) -> CompileResult<()> {
        match self.compile_unpack_sequence(expr_list)? {
            Unpack::Empty => self.emit_instr(OpCode::Empty),
            
            Unpack::Static(len) => {
                if let Ok(len) = u8::try_from(len) {
                    self.emit_instr_byte(OpCode::Tuple, len);
                } else {
                    self.compile_integer(len)?;
                    self.emit_instr(OpCode::TupleN);
                }
            }
            
            Unpack::Dynamic => {
                self.emit_instr(OpCode::TupleN);
            }
        }
        Ok(())
    }
    
    // compiles to a sequence of values
    fn compile_unpack_sequence(&mut self, seq: &[ExprMeta]) -> CompileResult<Unpack> {
        if seq.is_empty() {
            return Ok(Unpack::Empty)
        }
        
        let (last, rest) = seq.split_last().unwrap();
        
        let mut static_len: IntType = 0;
        let mut unpack_len = None; // accumulate length in an anonymous temporary if needed
        
        for expr in rest.iter() {
            match expr.variant() {
                Expr::Unpack(None) => return Err("need a value to unpack".into()),
                
                Expr::Unpack(Some(unpack)) => {
                    let symbol = expr.debug_symbol();
                    self.push_symbol(Some(*symbol));
                    
                    self.compile_expr(unpack)?;
                    self.emit_instr(OpCode::IterInit);
                    self.emit_instr(OpCode::IterUnpack);
                    
                    // store unpack len in accumulator
                    if let Some(local_index) = unpack_len {
                        self.emit_load_local_index(local_index);
                        self.emit_instr(OpCode::Add);
                        self.emit_assign_local(local_index);
                    } else {
                        self.emit_begin_scope(None, ScopeTag::Temporary);
                        let local_index = self.emit_create_temporary(Access::ReadWrite)?;
                        unpack_len = Some(local_index);
                    }
                    
                    self.emit_instr(OpCode::Pop);
                    self.pop_symbol();
                }
                
                _ => {
                    self.compile_expr_with_symbol(expr)?;
                    static_len = static_len.checked_add(1)
                        .ok_or("unpack length limit exceeded")?;
                }
            }
        }
        
        // if the last item is an unpack expression, it does not need to use the local accumulator
        // TODO should there be a dedicated accumulator register?
        match last.variant() {
            Expr::Unpack(None) => return Err("need a value to unpack".into()),
            
            Expr::Unpack(Some(unpack)) => {
                let symbol = last.debug_symbol();
                self.push_symbol(Some(*symbol));
                
                self.compile_expr(unpack)?;
                self.emit_instr(OpCode::IterInit);
                self.emit_instr(OpCode::IterUnpack);
                
                if let Some(local_index) = unpack_len {
                    self.emit_load_local_index(local_index);
                    self.emit_instr(OpCode::Add);
                    
                    debug_assert!(self.scopes().is_temporary_scope());
                    self.emit_end_scope();
                }
                
                if static_len > 0 {
                    self.compile_integer(static_len)?;
                    self.emit_instr(OpCode::Add);
                }
                
                self.pop_symbol();
                Ok(Unpack::Dynamic)
            }
            
            _ => {
                self.compile_expr_with_symbol(last)?;
                static_len = static_len.checked_add(1)
                    .ok_or("unpack length limit exceeded")?;
                    
                if let Some(local_index) = unpack_len {
                    self.emit_load_local_index(local_index);
                    
                    if static_len > 0 {
                        self.compile_integer(static_len)?;
                        self.emit_instr(OpCode::Add);
                    }
                    
                    debug_assert!(self.scopes().is_temporary_scope());
                    self.emit_end_scope();
                    
                    return Ok(Unpack::Dynamic)
                }
                
                Ok(Unpack::Static(static_len))
            }
        }
    }

    fn compile_atom(&mut self, atom: &Atom) -> CompileResult<()> {
        match atom {
            Atom::Nil => self.emit_instr(OpCode::Nil),
            Atom::EmptyTuple => self.emit_instr(OpCode::Empty),
            Atom::BooleanLiteral(true) => self.emit_instr(OpCode::True),
            Atom::BooleanLiteral(false) => self.emit_instr(OpCode::False),
            
            Atom::IntegerLiteral(value) => self.compile_integer(*value)?,
            Atom::FloatLiteral(value) => self.compile_float(*value)?,
            
            Atom::StringLiteral(value) => self.emit_load_const(Constant::from(*value))?,
            Atom::Identifier(name) => self.compile_name_lookup(name)?,
            
            // Atom::Self_ => unimplemented!(),
            // Atom::Super => unimplemented!(),
            
            Atom::Group { modifier, inner } => {
                // modifiers are not allowed outside of assignment
                if modifier.is_some() {
                    return Err("assignment modifiers are not allowed outside of an assignment expression".into())
                }
                
                match &**inner {
                    // tuple constructor
                    Expr::Unpack(None) => return Err("need a value to unpack".into()),
                    Expr::Unpack(Some(iter)) => {
                        self.compile_expr(iter)?;
                        self.emit_instr(OpCode::IterInit);
                        self.emit_instr(OpCode::IterUnpack);
                        self.emit_instr(OpCode::TupleN);
                    }
                    
                    // parenthesized group
                    _ => self.compile_expr(inner)?,
                }
                
            },
        }
        Ok(())
    }
    
    fn compile_integer(&mut self, value: IntType) -> CompileResult<()> {
        if let Ok(value) = u8::try_from(value) {
            self.emit_instr_byte(OpCode::UInt8, value);
        } else if let Ok(value) = i8::try_from(value) {
            self.emit_instr_byte(OpCode::Int8, value.to_le_bytes()[0]);
        } else if let Ok(value) = i16::try_from(value) {
            self.emit_instr_data(OpCode::Int16, &value.to_le_bytes());
        } else {
            self.emit_load_const(Constant::from(value))?;
        }
        Ok(())
    }
    
    fn compile_float(&mut self, value: FloatType) -> CompileResult<()> {
        self.emit_load_const(Constant::from(value))
    }
    
    fn compile_name_lookup(&mut self, name: &InternSymbol) -> CompileResult<()> {
        let local_name = LocalName::Symbol(*name);
        
        // Try loading a Local variable
        if self.try_emit_load_local(&local_name).is_some() {
            return Ok(());
        }
        
        // Next, try loading an upvalue
        if self.try_emit_load_upval(&local_name)?.is_some() {
            return Ok(());
        }
        
        // Otherwise, it must be a Global variable
        self.emit_load_const(Constant::from(*name))?;
        self.emit_instr(OpCode::LoadGlobal);
        Ok(())
    }
    
    fn compile_primary(&mut self, primary: &Primary) -> CompileResult<()> {
        self.compile_atom(primary.atom())?;
        
        for item in primary.path().iter() {
            match item {
                AccessItem::Attribute(_name) => unimplemented!(),
                AccessItem::Index(_index) => unimplemented!(),
                AccessItem::Invoke(args) => self.compile_invocation(args)?,
            }
        }
        
        Ok(())
    }
    
    fn compile_invocation(&mut self, args: &[ExprMeta]) -> CompileResult<()> {
        // prepare argument list:
        // [ callobj arg[0] ... arg[n] nargs ] => [ ret_value ] 

        // process argument unpacking
        match self.compile_unpack_sequence(args)? {
            Unpack::Empty => self.emit_instr_byte(OpCode::UInt8, 0),
            Unpack::Static(len) => self.compile_integer(len)?,
            Unpack::Dynamic => { } // nothing to do
        }

        self.emit_instr(OpCode::Call);

        Ok(())
    }
    
    fn compile_unary_op(&mut self, op: UnaryOp, expr: &Expr) -> CompileResult<()> {
        self.compile_expr(expr)?;
        match op {
            UnaryOp::Neg => self.emit_instr(OpCode::Neg),
            UnaryOp::Pos => self.emit_instr(OpCode::Pos),
            UnaryOp::Inv => self.emit_instr(OpCode::Inv),
            UnaryOp::Not => self.emit_instr(OpCode::Not),
        };
        Ok(())
    }
    
    fn compile_binary_op(&mut self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> CompileResult<()> {
        
        if matches!(op, BinaryOp::And) {
            return self.compile_shortcircuit_and(lhs, rhs);
        }
        if matches!(op, BinaryOp::Or) {
            return self.compile_shortcircuit_or(lhs, rhs);
        }
        
        self.compile_expr(lhs)?;
        self.compile_expr(rhs)?;
        self.emit_binary_op(op);
        
        Ok(())
    }
    
    fn emit_binary_op(&mut self, op: BinaryOp) {
        match op {
            BinaryOp::And | BinaryOp::Or => unreachable!(),
            
            BinaryOp::Mul => self.emit_instr(OpCode::Mul),
            BinaryOp::Div => self.emit_instr(OpCode::Div),
            BinaryOp::Mod => self.emit_instr(OpCode::Mod),
            BinaryOp::Add => self.emit_instr(OpCode::Add),
            BinaryOp::Sub => self.emit_instr(OpCode::Sub),
            
            BinaryOp::BitAnd => self.emit_instr(OpCode::And),
            BinaryOp::BitXor => self.emit_instr(OpCode::Xor),
            BinaryOp::BitOr  => self.emit_instr(OpCode::Or),
            
            BinaryOp::LShift  => self.emit_instr(OpCode::Shl),
            BinaryOp::RShift => self.emit_instr(OpCode::Shr),
            
            BinaryOp::LT => self.emit_instr(OpCode::LT),
            BinaryOp::GT => self.emit_instr(OpCode::GT),
            BinaryOp::LE => self.emit_instr(OpCode::LE),
            BinaryOp::GE => self.emit_instr(OpCode::GE),
            BinaryOp::EQ => self.emit_instr(OpCode::EQ),
            BinaryOp::NE => self.emit_instr(OpCode::NE),
        };
    }
}

///////// Declarations and Assignments /////////
impl CodeGenerator<'_> {
    fn compile_update_assignment(&mut self, op: BinaryOp, action: MatchAction, lhs: &Pattern, rhs: &Expr) -> CompileResult<()> {
        
        let local_only = match action {
            MatchAction::AssignLocal => true,
            MatchAction::AssignNonLocal => false,
            
            MatchAction::DeclImmutable | MatchAction::DeclMutable
                => return Err("update-assignment is invalid when declaring a variable".into()),
        };
        
        // TODO suport Attribute and Index LValues as well
        match lhs {
            Pattern::Identifier(name) => {
                self.compile_name_lookup(name)?;
                self.compile_expr(rhs)?;
                self.emit_binary_op(op);
                
                self.compile_assign_identifier(name, local_only)
            },
            
            Pattern::Attribute(_target) => unimplemented!(),
            
            Pattern::Index(_target) => unimplemented!(),
            
            Pattern::Tuple {..} | Pattern::Pack(..)
                => Err("can't update-assign to this".into()),
            
            Pattern::Modifier {..} => unreachable!(),
        }
    }
    
    fn compile_assignment(&mut self, mut action: MatchAction, mut lhs: &Pattern) -> CompileResult<()> {
        
        while let Pattern::Modifier { modifier, pattern } = lhs {
            action = *modifier;
            lhs = pattern;
        }
        
        match lhs {
            Pattern::Tuple(items) => self.compile_assign_tuple(action, items),
            
            Pattern::Pack(..) => {
                let item = std::slice::from_ref(lhs);
                self.compile_assign_tuple(action, item)
            }
            
            lhs => {
                match action {
                    MatchAction::AssignLocal => self.compile_assign_variable(lhs, false),
                    MatchAction::AssignNonLocal => self.compile_assign_variable(lhs, true),
                    MatchAction::DeclImmutable => self.compile_decl_variable(Access::ReadOnly, lhs),
                    MatchAction::DeclMutable => self.compile_decl_variable(Access::ReadWrite, lhs),
                }
            }
        }
    }

    fn compile_decl_variable(&mut self, access: Access, lhs: &Pattern) -> CompileResult<()> {
        match lhs {
            Pattern::Identifier(name) => if self.scopes().is_global_scope() {
                self.compile_decl_global_name(access, *name)
            } else {
                self.compile_decl_local_name(access, *name)
            },
            
            Pattern::Tuple {..} => unreachable!(),
            Pattern::Modifier {..} => unreachable!(),
            
            _ => Err("not a variable name".into()),
        }
    }
    
    fn compile_decl_global_name(&mut self, access: Access, name: InternSymbol) -> CompileResult<()> {
        
        self.emit_load_const(Constant::from(name))?;
        match access {
            Access::ReadOnly => self.emit_instr(OpCode::InsertGlobal),
            Access::ReadWrite => self.emit_instr(OpCode::InsertGlobalMut),
        }
        Ok(())
    }
    
    fn compile_decl_local_name(&mut self, access: Access, name: InternSymbol) -> CompileResult<()> {
        
        match self.scopes_mut().insert_local(access, LocalName::Symbol(name))? {
            InsertLocal::CreateNew(..) => 
                self.emit_instr(OpCode::InsertLocal),
            
            InsertLocal::HideExisting(local_index) =>
                self.emit_assign_local(local_index),
        }
        
        Ok(())
    }
    
    fn compile_assign_variable(&mut self, lhs: &Pattern, allow_nonlocal: bool) -> CompileResult<()> {
        
        match lhs {
            Pattern::Identifier(name) => self.compile_assign_identifier(name, allow_nonlocal),
            
            Pattern::Attribute(_target) => unimplemented!(),
            
            Pattern::Index(_target) => unimplemented!(),
            
            _ => panic!("invalid assignment target"),
        }
    }
    
    fn compile_assign_identifier(&mut self, name: &InternSymbol, allow_nonlocal: bool) -> CompileResult<()> {
        
        // Generate assignment
        
        if !self.scopes().is_global_scope() {
            
            let local_name = LocalName::Symbol(*name);
            
            // check if the name is found in the local scope...
            let result = self.scopes().resolve_local(&local_name);
            
            if let Some(local) = result.cloned() {
                if !local.mode().can_write() {
                    return Err("can't assign to immutable local variable".into());
                }
                
                self.emit_assign_local(local.index());
                
                return Ok(());
            }
            
            // nonlocal keyword is not required in the global frame
            if !allow_nonlocal && self.scopes().is_call_frame() {
                return Err("can't assign to a non-local variable without the \"nonlocal\" keyword".into());
            }
            
            // check if an upvalue is found or can be created...
            if self.scopes().is_call_frame() {
                if let Some(upval) = self.scopes_mut().resolve_or_create_upval(&local_name)? {
                    if !upval.mode().can_write() {
                        return Err("can't assign to immutable local variable".into());
                    }
                    
                    let index = upval.index();
                    if let Ok(index) = u8::try_from(index) {
                        self.emit_instr_byte(OpCode::StoreUpvalue, index);
                    } else {
                        self.emit_instr_data(OpCode::StoreUpvalue16, &index.to_le_bytes());
                    }
                    
                    return Ok(());
                }
            }
        }

        // ...finally, try to assign to a global, which are late bound
        self.emit_load_const(Constant::from(*name))?;
        self.emit_instr(OpCode::StoreGlobal);
        Ok(())
    }
    
    fn emit_assign_local(&mut self, offset: LocalIndex) {
        if let Ok(offset) = u8::try_from(offset) {
            self.emit_instr_byte(OpCode::StoreLocal, offset);
        } else {
            self.emit_instr_data(OpCode::StoreLocal16, &offset.to_le_bytes());
        }
    }
    
    fn compile_assign_tuple(&mut self, action: MatchAction, item_targets: &[Pattern]) -> CompileResult<()> {
        // process tuple packing patterns
        
        let mut pack_targets = item_targets.iter().enumerate()
            .filter_map(|(idx, target)| match target {
                Pattern::Pack(pack_target) => Some((idx, pack_target.as_deref())),
                _ => None,
            });
            
        let (idx, pack_target) = match pack_targets.next() {
            Some(pack_target) => pack_target,
            None => return self.compile_assign_tuple_nopack(action, item_targets),
        };
        
        if !pack_targets.next().is_none() {
            return Err("tuple assignment may contain only one \"...\" pack pattern".into())
        }
        
        let (pre_pack, rest) = item_targets.split_at(idx);
        let (_, post_pack) = rest.split_at(1);
        
        self.compile_assign_tuple_pack(action, pack_target, pre_pack, post_pack)
    }
    
    fn compile_assign_tuple_pack(&mut self, action: MatchAction, pack: Option<&Pattern>, pre_pack: &[Pattern], post_pack: &[Pattern]) -> CompileResult<()> {
        let mut error_jump_sites = Vec::new();
        
        // assignment needs to preserve original value for expression result
        self.emit_instr(OpCode::Clone);
        
        // iterate to yield values
        self.emit_instr(OpCode::IterInit);
        
        // compile to unrolled iteration for pre-pack items
        for target in pre_pack.iter() {
            debug_assert!(!matches!(target, Pattern::Pack(..)));
            
            // check if there is an item left for this target
            let error_jump = self.emit_dummy_jump(Jump::IfFalse);
            error_jump_sites.push(error_jump);
            
            // advance the iterator and put the item on the stack
            self.emit_instr(OpCode::IterNext);
            
            self.compile_assignment(action, target)?;
            self.emit_instr(OpCode::Pop);
        }
        
        let temp_scope;
        match (pack, post_pack) {
            (None, []) => {
                // if there are no post-pack items and no pack target just discard the iterator
                self.emit_instr_byte(OpCode::Drop, 2);
                temp_scope = false;
            }
            
            (Some(pack_target), []) => {
                // exhaust the rest of the iterator and assign to pack_target
                self.emit_instr(OpCode::IterUnpack);
                self.emit_instr(OpCode::TupleN);
                
                self.compile_assignment(action, pack_target)?;
                self.emit_instr(OpCode::Pop);
                temp_scope = false;
            }
            
            (pack_target, post_pack) => {
                // exhaust the rest of the iterator and assign the last items to post_pack
                // then assign whatever is left to the pack_target
                self.emit_instr(OpCode::IterUnpack);
                
                // calculate the pack length and store it
                let post_len = IntType::try_from(post_pack.len())
                    .map_err(|_| "unpack length limit exceeded")?;
                
                self.compile_integer(post_len)?;
                self.emit_instr(OpCode::Sub);
                
                temp_scope = true;
                self.emit_begin_scope(None, ScopeTag::Temporary);
                let pack_len = self.emit_create_temporary(Access::ReadOnly)?;
                
                // check if there are enough items
                self.emit_instr_byte(OpCode::UInt8, 0);
                self.emit_instr(OpCode::LT);
                let error_jump = self.emit_dummy_jump(Jump::PopIfTrue);
                error_jump_sites.push(error_jump);
                
                // assign post-pack items
                for target in post_pack.iter().rev() {
                    debug_assert!(!matches!(target, Pattern::Pack(..)));
                    self.compile_assignment(action, target)?;
                    self.emit_instr(OpCode::Pop);
                }
                
                // load the stored pack len and assign to pack target or discard
                self.emit_load_local_index(pack_len);
                if let Some(pack_target) = pack_target {
                    self.emit_instr(OpCode::TupleN);
                    self.compile_assignment(action, pack_target)?;
                    self.emit_instr(OpCode::Pop);
                } else {
                    self.emit_instr(OpCode::DropN);
                }
            }
        }
        
        let done_jump_site = self.emit_dummy_jump(Jump::Uncond);
        
        // not enough items
        let error_target = self.current_offset();
        for jump_site in error_jump_sites.iter() {
            self.patch_jump_instr(jump_site, error_target)?;
        }
        
        let message = format!("not enough values to unpack (expected at least {})", pre_pack.len() + post_pack.len());
        self.emit_load_error(ErrorKind::UnpackError, message.as_str())?;
        self.emit_instr(OpCode::Error);
        
        // cleanup
        self.patch_jump_instr(&done_jump_site, self.current_offset())?;
        
        if temp_scope {
            debug_assert!(self.scopes().is_temporary_scope());
            self.emit_end_scope();
        }
        
        Ok(())
    }
    
    fn compile_assign_tuple_nopack(&mut self, action: MatchAction, items: &[Pattern]) -> CompileResult<()> {
        let mut error_jump_sites = Vec::new();
        
        // assignment needs to preserve original value for expression result
        self.emit_instr(OpCode::Clone);
        
        // iterate to yield values
        self.emit_instr(OpCode::IterInit);
        
        // compile to unrolled iteration
        for target in items.iter() {
            debug_assert!(!matches!(target, Pattern::Pack(..)));
            
            // check if there is an item left for this target
            let error_jump = self.emit_dummy_jump(Jump::IfFalse);
            error_jump_sites.push(error_jump);
            
            // advance the iterator and put the item on the stack
            self.emit_instr(OpCode::IterNext);
            
            self.compile_assignment(action, target)?;
            self.emit_instr(OpCode::Pop);
        }
        
        // if the iterator is finished we've succeeded
        let done_jump_site = self.emit_dummy_jump(Jump::PopIfFalse);
        
        // too many items
        let message = format!("too many values to unpack (expected {})", items.len());
        self.emit_load_error(ErrorKind::UnpackError, message.as_str())?;
        self.emit_instr(OpCode::Error);
        
        // not enough items
        let error_target = self.current_offset();
        for jump_site in error_jump_sites.iter() {
            self.patch_jump_instr(jump_site, error_target)?;
        }
        
        let message = format!("not enough values to unpack (expected {})", items.len());
        self.emit_load_error(ErrorKind::UnpackError, message.as_str())?;
        self.emit_instr(OpCode::Error);
        
        // cleanup
        self.patch_jump_instr(&done_jump_site, self.current_offset())?;
        
        self.emit_instr(OpCode::Pop);  // pop the iterator
        
        Ok(())
    }
}

///////// Blocks and If-Expressions /////////
impl CodeGenerator<'_> {

    fn compile_block_expression(&mut self, label: Option<&Label>, suite: &ExprBlock) -> CompileResult<()> {
        
        self.emit_begin_scope(label, ScopeTag::Block);
        self.compile_expr_block(suite)?;
        let block_scope = self.emit_end_scope();
        
        // finalize scope
        let break_target = self.current_offset();
        self.patch_break_sites(&block_scope, break_target)?;
        
        Ok(())
    }
    
    fn compile_if_expression(&mut self, branches: &[ConditionalBranch], else_clause: Option<&ExprBlock>) -> CompileResult<()> {
        debug_assert!(!branches.is_empty());
        
        // track the sites where we jump to the end, so we can patch them later
        let mut end_jump_sites = Vec::new();
        
        // if there is no else branch, the last non-else branch won't have a jump to end, and should not pop the condition
        // this is because if-expressions without an else clause evaluate to their condition when not entered
        let (last_branch, rest) = branches.split_last().unwrap();
        let iter_branches = rest.iter()
            .map(|branch| (false, branch))
            .chain(iter::once((true, last_branch)));
        
        for (is_last, branch) in iter_branches {
            let is_final_branch = is_last && else_clause.is_none();
            
            self.compile_expr(branch.condition())?;
            
            // need to keep condition value on the stack in case there is a break/continue
            // inside the statement list
            let branch_jump_site = self.emit_dummy_jump(Jump::IfFalse);
            
            self.emit_begin_scope(None, ScopeTag::Branch);
            self.compile_expr_block(branch.suite())?;
            self.emit_end_scope();
            
            // site for the jump to the end of if-expression
            if !is_final_branch {
                self.emit_instr(OpCode::Pop);
                let jump_site = self.emit_dummy_jump(Jump::Uncond);
                end_jump_sites.push(jump_site);
            }
            
            // target for the jump from the conditional of the now compiled branch
            self.patch_jump_instr(&branch_jump_site, self.current_offset())?;
        }
        
        // else clause
        if let Some(suite) = else_clause {
            
            self.emit_begin_scope(None, ScopeTag::Branch);
            self.compile_expr_block(suite)?;
            self.emit_end_scope();
            
        }
        
        // patch all of the end jump sites
        let end_target = self.current_offset();
        for jump_site in end_jump_sites.iter() {
            self.patch_jump_instr(jump_site, end_target)?;
        }
        
        Ok(())
    }
    
    fn compile_shortcircuit_and(&mut self, lhs: &Expr, rhs: &Expr) -> CompileResult<()> {
        self.compile_expr(lhs)?;
        
        let shortcircuit = self.emit_dummy_jump(Jump::IfFalse);
        
        self.emit_instr(OpCode::Pop);
        self.compile_expr(rhs)?;
        
        self.patch_jump_instr(&shortcircuit, self.current_offset())?;
        
        Ok(())
    }
    
    fn compile_shortcircuit_or(&mut self, lhs: &Expr, rhs: &Expr) -> CompileResult<()> {
        self.compile_expr(lhs)?;
        
        let shortcircuit = self.emit_dummy_jump(Jump::IfTrue);
        
        self.emit_instr(OpCode::Pop);
        self.compile_expr(rhs)?;
        
        self.patch_jump_instr(&shortcircuit, self.current_offset())?;
        
        Ok(())
    }
}

///////// Function Definitions /////////
impl CodeGenerator<'_> {
    fn compile_function_def(&mut self, fundef: &FunctionDef) -> CompileResult<()> {
        // create a new chunk for the function
        let symbol = self.current_symbol();
        let info = ChunkInfo::Function { symbol };

        let mut chunk_gen = self.create_chunk(info)?;
        
        let chunk_id = chunk_gen.chunk_id();
        let fun_id = match chunk_id {
            Chunk::Function(fun_id) => fun_id,
            _ => panic!("chunk {:?} is not valid for function", chunk_id),
        };
        
        // and a new local scope
        // don't need to emit new scope instructions, should handled by function call
        chunk_gen.scopes_mut().push_frame(symbol.as_ref());
        
        // don't need to generate IN_LOCAL instructions for these, the VM should include them automatically
        chunk_gen.scopes_mut().insert_local(Access::ReadOnly, LocalName::Receiver)?;
        chunk_gen.scopes_mut().insert_local(Access::ReadOnly, LocalName::NArgs)?;
        
        // prepare argument list
        chunk_gen.compile_function_preamble(fundef)?;
        
        // function body
        chunk_gen.compile_stmt_block(fundef.body.stmt_list())?;
        
        // function result
        if let Some(expr) = fundef.body.result() {
            chunk_gen.compile_expr_with_symbol(expr)?;
        } else {
            chunk_gen.emit_instr(OpCode::Nil);
        }
        
        // end the function scope
        // don't need to drop locals explicitly, that will be done when the VMCallFrame returns
        let frame = chunk_gen.scopes_mut().pop_frame();
        
        // however we do still need to close upvalues before we return
        for local in frame.iter_locals().filter(|local| local.captured()) {
            chunk_gen.emit_close_upvalue(local.index());
        }
        
        chunk_gen.finish();
        
        // compile the function signature
        let signature = self.compile_function_signature(&fundef.signature)?;
        
        // compile upvalues
        let upvalues = frame.upvalues().iter()
            .map(|upval| upval.target())
            .collect::<Vec<UpvalueTarget>>()
            .into_boxed_slice();
        
        let function = UnloadedFunction {
            signature, upvalues, fun_id,
        };
        
        // load the function object as the expression result
        self.make_function(function);
        self.emit_load_function(fun_id);
        
        Ok(())
    }
    
    fn compile_function_preamble(&mut self, fundef: &FunctionDef) -> CompileResult<()> {
        // process default and variadic arguments
        // this ensures that exactly `signature.param_count()` values are on the stack
        
        let signature = &fundef.signature;
        if signature.param_count() == 0 {
            return Ok(())
        }
        
        for param in signature.required.iter() {
            self.scopes_mut().insert_local(param.mode, LocalName::Symbol(param.name))?;
        }
        
        if !signature.default.is_empty() {
            self.compile_default_args(signature)?;
            for param in signature.default.iter() {
                self.scopes_mut().insert_local(param.mode, LocalName::Symbol(param.name))?;
            }
        }
        
        if let Some(param) = &signature.variadic {
            self.compile_variadic_arg(signature)?;
            self.scopes_mut().insert_local(param.mode, LocalName::Symbol(param.name))?;
        }
        
        self.emit_instr(OpCode::InsertArgs);

        Ok(())
    }
    
    fn compile_default_args(&mut self, signature: &SignatureDef) -> CompileResult<()> {
        debug_assert!(!signature.default.is_empty());
        
        let mut jump_sites = Vec::new();
        let mut jump_targets = Vec::new();
        
        // depending on the number of arguments, jump into the default argument sequence
        let required_count = u8::try_from(signature.required.len())
            .map_err(|_| "parameter count limit exceeded")?;
        let default_count = u8::try_from(signature.default.len())
            .map_err(|_| "parameter count limit exceeded")?;
        
        // "defaults passed" = NArgs - required_count
        self.try_emit_load_local(&LocalName::NArgs).unwrap();
        if required_count != 0 {
            self.emit_instr_byte(OpCode::UInt8, required_count);
            self.emit_instr(OpCode::Sub);
        }
        
        /*
            NArgs - required count:
            
            <  0   => impossible, missing required argument
               0   => jump to default[0]
               1   => jump to default[1]
               ...
               N-1 => jump to default[N-1]
            >= N   => jump to end
        */
        
        for count in 0..default_count {
            self.emit_instr(OpCode::Clone);
            self.emit_instr_byte(OpCode::UInt8, count);
            self.emit_instr(OpCode::EQ);
            
            let jump_site = self.emit_dummy_jump(Jump::PopIfTrue);
            jump_sites.insert(count.into(), jump_site);
        }
        
        // if we get here, jump unconditionally to the end
        let jump_site = self.emit_dummy_jump(Jump::Uncond);
        jump_sites.insert(default_count.into(), jump_site);
        
        // generate default arguments
        for (idx, param) in signature.default.iter().enumerate() {
            jump_targets.insert(idx, self.current_offset());
            
            let symbol = param.default.debug_symbol();
            let expr = param.default.variant();
            self.push_symbol(Some(*symbol));
            self.compile_expr(expr)?;
            self.pop_symbol();
        }
        
        jump_targets.insert(default_count.into(), self.current_offset());
        self.emit_instr(OpCode::Pop);  // drop "defaults passed"
        
        // patch all jumps
        debug_assert!(jump_sites.len() == jump_targets.len());
        for (jump_site, jump_target) in jump_sites.iter().zip(jump_targets.into_iter()) {
            self.patch_jump_instr(jump_site, jump_target)?;
        }
        
        Ok(())
    }
    
    fn compile_variadic_arg(&mut self, signature: &SignatureDef) -> CompileResult<()> {
        debug_assert!(signature.variadic.is_some());
        
        let positional_count = u8::try_from(signature.required.len() + signature.default.len())
            .map_err(|_| "parameter count limit exceeded")?;
        
        // "variadic count" = NArgs - required_count - default_count
        self.try_emit_load_local(&LocalName::NArgs).unwrap();
        if positional_count != 0 {
            self.emit_instr_byte(OpCode::UInt8, positional_count);
            self.emit_instr(OpCode::Sub);
        }
        
        // check if "variadic count" > 0
        self.emit_instr(OpCode::Clone);
        self.emit_instr_byte(OpCode::UInt8, 0);
        self.emit_instr(OpCode::GT);
        
        let tuple_jump_site = self.emit_dummy_jump(Jump::PopIfTrue);
        
        // if we get here then the variadic arg is empty
        self.emit_instr(OpCode::Pop);
        self.emit_instr(OpCode::Empty);
        
        let end_jump_site = self.emit_dummy_jump(Jump::Uncond);
        
        self.patch_jump_instr(&tuple_jump_site, self.current_offset())?;
        self.emit_instr(OpCode::TupleN);
        
        self.patch_jump_instr(&end_jump_site, self.current_offset())?;
        
        Ok(())
    }
    
    fn compile_function_signature(&mut self, signature: &SignatureDef) -> CompileResult<UnloadedSignature> {
        let name = 
            if let Some(name) = signature.name {
                Some(self.get_or_make_const(Constant::from(name))?)
            } else { None };
        
        let mut required = Vec::new();
        for param in signature.required.iter() {
            let name = self.get_or_make_const(Constant::from(param.name))?;
            required.push(UnloadedParam { name, mode: param.mode });
        }
        
        let mut default = Vec::new();
        for param in signature.default.iter() {
            let name = self.get_or_make_const(Constant::from(param.name))?;
            default.push(UnloadedParam { name, mode: param.mode });
        }
        
        let mut variadic = None;
        if let Some(param) = &signature.variadic {
            let name = self.get_or_make_const(Constant::from(param.name))?;
            variadic.replace(UnloadedParam { name, mode: param.mode });
        }
        
        let signature = UnloadedSignature {
            name,
            required: required.into_boxed_slice(),
            default: default.into_boxed_slice(),
            variadic,
        };
        
        Ok(signature)
    }
    
}
