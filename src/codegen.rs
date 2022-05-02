#![allow(unused_variables)]

use core::iter;

use crate::language::{IntType, FloatType, InternSymbol};
use crate::parser::stmt::{StmtMeta, Stmt, Label, StmtList, ControlFlow};
use crate::parser::expr::{Expr, ExprMeta, ExprBlock, ConditionalBranch};
use crate::parser::primary::{Atom, Primary, AccessItem};
use crate::parser::lvalue::{LValue, DeclType};
use crate::parser::fundefs::{FunctionDef, SignatureDef};
use crate::parser::operator::{UnaryOp, BinaryOp};
use crate::runtime::strings::{StringInterner};
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
pub use errors::{CompileResult, CompileError, ErrorKind};

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
        CodeGenerator {
            compiler: self,
            chunk_id,
        }
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
        match self.chunk_id {
            Chunk::Main => self.emit_instr(None, OpCode::Exit),
            
            Chunk::Function(..) => self.emit_instr(None, OpCode::Return),
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
    
    fn push_symbol(&mut self, symbol: DebugSymbol) {
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
    fn emit_instr(&mut self, symbol: Option<&DebugSymbol>, opcode: OpCode) {
        debug_assert!(opcode.instr_len() == 1);
        
        if let Some(symbol) = symbol {
            self.push_symbol(*symbol);
        }
        
        self.chunk_mut().push_byte(opcode);
    }
    
    fn emit_instr_byte(&mut self, symbol: Option<&DebugSymbol>, opcode: OpCode, byte: u8) {
        debug_assert!(opcode.instr_len() == 2);
        
        if let Some(symbol) = symbol {
            self.push_symbol(*symbol);
        }
        
        self.chunk_mut().push_byte(opcode);
        self.chunk_mut().push_byte(byte);
    }
    
    fn emit_instr_data(&mut self, symbol: Option<&DebugSymbol>, opcode: OpCode, bytes: &[u8]) {
        debug_assert!(opcode.instr_len() == 1 + bytes.len());
        
        if let Some(symbol) = symbol {
            self.push_symbol(*symbol);
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
    
    fn emit_dummy_instr(&mut self, symbol: Option<&DebugSymbol>, width: usize) {
        if let Some(symbol) = symbol {
            self.push_symbol(*symbol);
        }
        
        for i in 0..width {
            self.chunk_mut().push_byte(OpCode::Nop);
        }
    }
}

///////// Constants /////////
impl CodeGenerator<'_> {
    fn get_or_make_const(&mut self, value: Constant) -> CompileResult<ConstID> {
        self.builder_mut().get_or_insert_const(value)
    }
    
    fn emit_load_const(&mut self, symbol: Option<&DebugSymbol>, value: Constant) -> CompileResult<()> {
        let cid = self.get_or_make_const(value)?;
        
        if cid <= u8::MAX.into() {
            self.emit_instr_byte(symbol, OpCode::LoadConst, u8::try_from(cid).unwrap());
        } else {
            self.emit_instr_data(symbol, OpCode::LoadConst16, &cid.to_le_bytes());
        }
        Ok(())
    }
    
    fn make_function(&mut self, function: UnloadedFunction) {
        self.builder_mut().insert_function(function)
    }
    
    fn emit_load_function(&mut self, symbol: Option<&DebugSymbol>, fun_id: FunctionID) {
        if fun_id <= u8::MAX.into() {
            self.emit_instr_byte(symbol, OpCode::LoadFunction, u8::try_from(fun_id).unwrap());
        } else {
            self.emit_instr_data(symbol, OpCode::LoadFunction16, &fun_id.to_le_bytes());
        }
    }
}

///////// Jumps /////////
impl CodeGenerator<'_> {
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
                return Err(ErrorKind::InternalLimit("could not calculate jump offset").into());
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
    
    fn emit_dummy_jump(&mut self, symbol: Option<&DebugSymbol>, jump: Jump) -> JumpSite {
        let offset = self.current_offset();
        let jump_site = JumpSite {
            jump, offset,
            width: jump.dummy_width(),
        };
        
        self.emit_dummy_instr(symbol, jump_site.width);
        
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
                return Err(ErrorKind::InternalLimit("could not calculate jump offset").into());
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
        
        Err(ErrorKind::InternalLimit("could not calculate jump offset").into())
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
    fn emit_begin_scope(&mut self, symbol: Option<&DebugSymbol>, tag: ScopeTag, label: Option<&Label>) {
        let continue_target = self.current_offset();
        
        let chunk_id = self.chunk_id;
        self.scopes_mut().push_scope(symbol, tag, label.copied());
        
        self.scopes_mut().local_scope_mut().unwrap()
            .set_continue(continue_target);
    }
    
    fn emit_end_scope(&mut self) -> Scope {
        let scope = self.scopes_mut().pop_scope();
        self.emit_scope_drop(scope.debug_symbol(), &(&scope).into());
        scope
    }
    
    fn finalize_scope(&mut self, scope: &Scope, break_target: usize) -> CompileResult<()> {
        for break_site in scope.break_sites().iter() {
            self.patch_jump_instr(break_site, break_target)?;
        }
        Ok(())
    }
    
    fn emit_scope_drop(&mut self, symbol: Option<&DebugSymbol>, scope: &ScopeDrop) {
        // close all upvalues
        for local_index in scope.close_upvals.iter() {
            self.emit_close_upvalue(symbol, *local_index);
        }
        
        // discard all the locals from the stack
        let mut discard = scope.locals;
        while discard > u8::MAX.into() {
            self.emit_instr_byte(symbol, OpCode::DropLocals, u8::MAX);
            discard -= usize::from(u8::MAX);
        }
        
        if discard > 0 {
            self.emit_instr_byte(symbol, OpCode::DropLocals, u8::try_from(discard).unwrap());
        }
    }
    
    // If the local name cannot be found, no instructions are emitted and None is returned
    fn try_emit_load_local(&mut self, symbol: Option<&DebugSymbol>, name: &LocalName) -> Option<u16> {
        if let Some(index) = self.scopes().resolve_local(name).map(|local| local.index()) {
            if let Ok(index) = u8::try_from(index) {
                self.emit_instr_byte(symbol, OpCode::LoadLocal, index);
            } else {
                self.emit_instr_data(symbol, OpCode::LoadLocal16, &index.to_le_bytes());
            }
            
            Some(index)
        } else{
            None
        }
    }
    
    fn try_emit_load_upval(&mut self, symbol: Option<&DebugSymbol>, name: &LocalName) -> CompileResult<Option<u16>> {
        if let Some(index) = self.scopes_mut().resolve_or_create_upval(name)?.map(|upval| upval.index()) {
            if let Ok(index) = u8::try_from(index) {
                self.emit_instr_byte(symbol, OpCode::LoadUpvalue, index);
            } else {
                self.emit_instr_data(symbol, OpCode::LoadUpvalue16, &index.to_le_bytes());
            }
            
            Ok(Some(index))
        } else {
            Ok(None)
        }
    }
    
    fn emit_close_upvalue(&mut self, symbol: Option<&DebugSymbol>, index: LocalIndex) {
        if let Ok(index) = u8::try_from(index) {
            self.emit_instr_byte(symbol, OpCode::CloseUpvalue, index);
        } else {
            self.emit_instr_data(symbol, OpCode::CloseUpvalue16, &index.to_le_bytes());
        }
    }
}


///////// Statements /////////
impl CodeGenerator<'_> {
    fn compile_stmt_with_symbol(&mut self, stmt: &StmtMeta) -> CompileResult<()> {
        let symbol = stmt.debug_symbol();
        self.compile_stmt(Some(symbol), stmt.variant())
            .map_err(|err| err.with_symbol(*symbol))
    }
    
    fn compile_stmt(&mut self, symbol: Option<&DebugSymbol>, stmt: &Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::Loop { label, body } => self.compile_loop(symbol, label.as_ref(), body)?,
            
            Stmt::WhileLoop { label, condition, body } => self.compile_while_loop(symbol, label.as_ref(), condition, body)?,
            
            Stmt::ForLoop { } => unimplemented!(),
            
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
        self.compile_end_control(stmt_list)?;
        Ok(())
    }
    
    fn compile_expr_block(&mut self, symbol: Option<&DebugSymbol>, suite: &ExprBlock) -> CompileResult<()> {
        let stmt_list = suite.stmt_list();
        self.compile_stmt_list(stmt_list)?;
        
        if stmt_list.end_control().is_none() {
            // result expression
            if let Some(expr) = suite.result() {
                self.compile_expr_with_symbol(expr)?;
            } else {
                self.emit_instr(symbol, OpCode::Nil); // implicit nil
            }
        }
        
        self.compile_end_control(stmt_list)?;
        
        Ok(())
    }
    
    fn compile_end_control(&mut self, stmt_list: &StmtList) -> CompileResult<()> {
        // handle control flow
        if let Some(control) = stmt_list.end_control() {
            let result = self.compile_control_flow(control);
            if let Some(symbol) = control.debug_symbol() {
                result.map_err(|error| error.with_symbol(*symbol))?;
            } else {
                result?;
            }
        }
        Ok(())
    }
    
    fn compile_control_flow(&mut self, control_flow: &ControlFlow) -> CompileResult<()> {
        match control_flow {
            ControlFlow::Continue { label, symbol } => self.compile_continue_control(
                symbol.as_ref(), label.as_ref()
            )?,
            
            ControlFlow::Break { label, expr, symbol } => self.compile_break_control(
                symbol.as_ref(), label.as_ref(), expr.as_deref()
            )?,
            
            ControlFlow::Return { expr, symbol } => {
                let symbol = symbol.as_ref();
                if let Some(expr) = expr {
                    self.compile_expr(symbol, expr)?;
                } else {
                    self.emit_instr(symbol, OpCode::Nil);
                }
                self.emit_instr(symbol, OpCode::Return);
            }
        }
        Ok(())
    }
    
    fn compile_break_control(&mut self, symbol: Option<&DebugSymbol>, label: Option<&Label>, expr: Option<&Expr>) -> CompileResult<()> {
        // find the target scope
        let target_depth = match self.scopes().resolve_control_flow(ControlFlowTarget::Break(label.copied())) {
            Some(scope) => scope.depth(),
            None => return Err(ErrorKind::CantResolveBreak(label.copied()).into()),
        };
        
        // drop all scopes up to and including the target
        let scope_drop: Vec<ScopeDrop> = self.scopes().iter_scopes()
            .take_while(|scope| scope.depth() >= target_depth)
            .map(ScopeDrop::from)
            .collect();
        
        let (target, through_scopes) = scope_drop.split_last().unwrap();
        for scope in through_scopes.iter() {
            self.emit_scope_drop(symbol, scope);
            
            // expression blocks leave their value on the stack
            // (this is helped by the fact that break/contine must come last in a list of statements)
            // so if we break through an expression block we need to pop its value
            if scope.tag.is_expr_block() {
                self.emit_instr(symbol, OpCode::Pop);
            }
        }
        self.emit_scope_drop(symbol, target); // drop target scope
        
        
        // if breaking from an expression block, emit the expression value before jumping
        if target.tag.is_expr_block() {
            if let Some(expr) = expr {
                self.compile_expr(symbol, expr)?;
            } else {
                self.emit_instr(symbol, OpCode::Nil);
            }
        } else if expr.is_some() {
            return Err(CompileError::new(ErrorKind::InvalidBreakWithValue))
        }
        
        // emit jump site, register with scope
        let break_site = self.emit_dummy_jump(symbol, Jump::Uncond);
        
        let target_scope = self.scopes_mut().iter_scopes_mut()
            .find(|scope| scope.depth() == target_depth)
            .unwrap();
        target_scope.register_break(break_site);
        
        Ok(())
        
    }
    
    fn compile_continue_control(&mut self, symbol: Option<&DebugSymbol>, label: Option<&Label>) -> CompileResult<()> {
        // find the target scope
        let (target_depth, continue_target) = match self.scopes().resolve_control_flow(ControlFlowTarget::Continue(label.copied())) {
            Some(scope) => (scope.depth(), scope.continue_target()),
            None => return Err(ErrorKind::CantResolveContinue(label.copied()).into()),
        };
        
        // drop all scopes up to and including the target
        let scope_drop: Vec<ScopeDrop> = self.scopes().iter_scopes()
            .take_while(|scope| scope.depth() >= target_depth)
            .map(ScopeDrop::from)
            .collect();
        
        for scope in scope_drop.iter() {
            self.emit_scope_drop(symbol, scope);
            
            // expression blocks leave their value on the stack
            // (this is helped by the fact that break/contine must come last in a list of statements)
            // so if we jump out of an expression block we need to pop its value
            if scope.tag.is_expr_block() {
                self.emit_instr(symbol, OpCode::Pop);
            }
        }
        
        // if continue_target is None by this point it's the compiler's fault
        self.emit_jump_instr(symbol, Jump::Uncond, continue_target.unwrap())?;
        
        Ok(())
    }
    
    fn compile_loop(&mut self, symbol: Option<&DebugSymbol>, label: Option<&Label>, body: &StmtList) -> CompileResult<()> {
        
        let loop_target = self.current_offset();
        
        self.emit_begin_scope(symbol, ScopeTag::Loop, label);
        self.compile_stmt_block(body)?;
        let loop_scope = self.emit_end_scope();
        
        self.emit_jump_instr(symbol, Jump::Uncond, loop_target)?;
        
        // finalize scope
        let break_target = self.current_offset();
        self.finalize_scope(&loop_scope, break_target)?;
        
        Ok(())
    }
    
    fn compile_while_loop(&mut self, symbol: Option<&DebugSymbol>, label: Option<&Label>, condition: &Expr, body: &StmtList) -> CompileResult<()> {
        
        // first iteration conditional jump
        self.compile_expr(symbol, condition)?;
        
        let end_jump_site = self.emit_dummy_jump(symbol, Jump::PopIfFalse);
        
        let loop_target = self.current_offset();
        
        self.emit_begin_scope(symbol, ScopeTag::Loop, label);
        self.compile_stmt_block(body)?;
        let loop_scope = self.emit_end_scope();
        
        // rest iteration conditional jump
        self.compile_expr(symbol, condition)?;
        self.emit_jump_instr(symbol, Jump::PopIfTrue, loop_target)?;
        
        self.patch_jump_instr(&end_jump_site, self.current_offset())?;
        
        // finalize scope
        let break_target = self.current_offset();
        self.finalize_scope(&loop_scope, break_target)?;
        
        Ok(())
    }
}

///////// Expressions /////////
impl CodeGenerator<'_> {
    fn compile_expr_with_symbol(&mut self, expr: &ExprMeta) -> CompileResult<()> {
        let symbol = expr.debug_symbol();
        self.compile_expr(Some(symbol), expr.variant())
            .map_err(|err| err.with_symbol(*symbol))
    }
    
    fn compile_expr(&mut self, symbol: Option<&DebugSymbol>, expr: &Expr) -> CompileResult<()> {
        match expr {
            Expr::Atom(atom) => self.compile_atom(symbol, atom)?,
            
            Expr::Primary(primary) => self.compile_primary(symbol, primary)?,
            
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
            
            Expr::Declaration(decl) => {
                self.compile_expr(symbol, &decl.init)?;
                self.compile_declaration(symbol, decl.decl, &decl.lhs)?;
            },
            
            Expr::Assignment(assign) => {
                if let Some(op) = assign.op {
                    self.compile_update_assignment(symbol, op, &assign.lhs, &assign.rhs, assign.nonlocal)?;
                } else {
                    self.compile_expr(symbol, &assign.rhs)?;
                    self.compile_assignment(symbol, &assign.lhs, assign.nonlocal)?;
                }
            },
            
            Expr::Tuple(expr_list) => self.compile_tuple(symbol, expr_list)?,
            
            Expr::Block { label, suite } => self.compile_block_expression(symbol, label.as_ref(), suite)?,
            Expr::IfExpr { branches, else_clause } => self.compile_if_expression(symbol, branches, else_clause.as_ref().map(|expr| &**expr))?,
            
            Expr::FunctionDef(fundef) => self.compile_function_def(symbol, fundef)?,
            
            Expr::Echo(expr) => {
                self.compile_expr(symbol, expr)?;
                self.emit_instr(symbol, OpCode::Inspect);
            },
        }
        Ok(())
    }
    
    fn compile_tuple(&mut self, symbol: Option<&DebugSymbol>, expr_list: &[ExprMeta]) -> CompileResult<()> {
        let len = u8::try_from(expr_list.len())
            .map_err(|_| ErrorKind::InternalLimit("tuple length limit exceeded"))?;
        
        for expr in expr_list.iter() {
            self.compile_expr_with_symbol(expr)?;
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
            
            Atom::IntegerLiteral(value) => self.compile_integer(symbol, *value)?,
            Atom::FloatLiteral(value) => self.compile_float(symbol, *value)?,
            
            Atom::StringLiteral(value) => self.emit_load_const(symbol, Constant::from(*value))?,
            Atom::Identifier(name) => self.compile_name_lookup(symbol, name)?,
            
            // Atom::Self_ => unimplemented!(),
            // Atom::Super => unimplemented!(),
            
            Atom::Group(expr) => self.compile_expr(symbol, expr)?,
        }
        Ok(())
    }
    
    fn compile_integer(&mut self, symbol: Option<&DebugSymbol>, value: IntType) -> CompileResult<()> {
        if let Ok(value) = u8::try_from(value) {
            self.emit_instr_byte(symbol, OpCode::UInt8, value);
        } else if let Ok(value) = i8::try_from(value) {
            self.emit_instr_byte(symbol, OpCode::Int8, value.to_le_bytes()[0]);
        } else if let Ok(value) = i16::try_from(value) {
            self.emit_instr_data(symbol, OpCode::Int16, &value.to_le_bytes());
        } else {
            self.emit_load_const(symbol, Constant::from(value))?;
        }
        Ok(())
    }
    
    fn compile_float(&mut self, symbol: Option<&DebugSymbol>, value: FloatType) -> CompileResult<()> {
        self.emit_load_const(symbol, Constant::from(value))
    }
    
    fn compile_name_lookup(&mut self, symbol: Option<&DebugSymbol>, name: &InternSymbol) -> CompileResult<()> {
        let local_name = LocalName::Symbol(*name);
        
        // Try loading a Local variable
        if self.try_emit_load_local(symbol, &local_name).is_some() {
            return Ok(());
        }
        
        // Next, try loading an upvalue
        if self.try_emit_load_upval(symbol, &local_name)?.is_some() {
            return Ok(());
        }
        
        // Otherwise, it must be a Global variable
        self.emit_load_const(symbol, Constant::from(*name))?;
        self.emit_instr(symbol, OpCode::LoadGlobal);
        Ok(())
    }
    
    fn compile_primary(&mut self, symbol: Option<&DebugSymbol>, primary: &Primary) -> CompileResult<()> {
        self.compile_atom(symbol, primary.atom())?;
        
        for item in primary.path().iter() {
            match item {
                AccessItem::Attribute(name) => unimplemented!(),
                AccessItem::Index(index) => unimplemented!(),
                AccessItem::Invoke { args, unpack } => self.compile_invocation(symbol, args, unpack.as_ref())?,
            }
        }
        
        Ok(())
    }
    
    fn compile_invocation(&mut self, symbol: Option<&DebugSymbol>, args: &[ExprMeta], unpack: Option<&ExprMeta>) -> CompileResult<()> {
        // prepare argument list:
        // [ callobj arg[n] arg[0] ... arg[n-1] nargs ] => [ ret_value ] 

        let arg_len;
        
        if let Some((arg_last, args)) = args.split_last() {
            
            self.compile_expr_with_symbol(arg_last)?;
            
            for arg_expr in args.iter() {
                self.compile_expr_with_symbol(arg_expr)?;
            }
            
            arg_len = u8::try_from(args.len() + 1)
                .map_err(|_| ErrorKind::InternalLimit("argument count limit exceeded"))?;
            
        } else {
            
            arg_len = 0;
        }
        
        if let Some(seq_expr) = unpack {
            self.compile_expr_with_symbol(seq_expr)?;
            self.emit_instr_byte(symbol, OpCode::UInt8, arg_len);
            self.emit_instr(symbol, OpCode::CallUnpack);
        } else {
            self.emit_instr_byte(symbol, OpCode::UInt8, arg_len);
            self.emit_instr(symbol, OpCode::Call);
        }

        Ok(())
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
            BinaryOp::And => unimplemented!(),
            BinaryOp::Or => unimplemented!(),
            
            BinaryOp::Mul => self.emit_instr(symbol, OpCode::Mul),
            BinaryOp::Div => self.emit_instr(symbol, OpCode::Div),
            BinaryOp::Mod => self.emit_instr(symbol, OpCode::Mod),
            BinaryOp::Add => self.emit_instr(symbol, OpCode::Add),
            BinaryOp::Sub => self.emit_instr(symbol, OpCode::Sub),
            
            BinaryOp::BitAnd => self.emit_instr(symbol, OpCode::And),
            BinaryOp::BitXor => self.emit_instr(symbol, OpCode::Xor),
            BinaryOp::BitOr  => self.emit_instr(symbol, OpCode::Or),
            
            BinaryOp::LShift  => self.emit_instr(symbol, OpCode::Shl),
            BinaryOp::RShift => self.emit_instr(symbol, OpCode::Shr),
            
            BinaryOp::LT => self.emit_instr(symbol, OpCode::LT),
            BinaryOp::GT => self.emit_instr(symbol, OpCode::GT),
            BinaryOp::LE => self.emit_instr(symbol, OpCode::LE),
            BinaryOp::GE => self.emit_instr(symbol, OpCode::GE),
            BinaryOp::EQ => self.emit_instr(symbol, OpCode::EQ),
            BinaryOp::NE => self.emit_instr(symbol, OpCode::NE),
        }
    }
}

///////// Declarations and Assignments /////////
impl CodeGenerator<'_> {
    fn compile_declaration(&mut self, symbol: Option<&DebugSymbol>, decl: DeclType, lhs: &LValue) -> CompileResult<()> {
        match lhs {
            LValue::Identifier(name) => if self.scopes().is_global_scope() {
                self.compile_decl_global_name(symbol, decl, *name)
            } else {
                self.compile_decl_local_name(symbol, decl, *name)
            },
            
            LValue::Attribute(target) => unimplemented!(),
            
            LValue::Index(target) => unimplemented!(),
            
            LValue::Tuple(target_list) => self.compile_decl_tuple(symbol, decl, target_list),
        }
    }
    
    fn compile_decl_local_name(&mut self, symbol: Option<&DebugSymbol>, decl: DeclType, name: InternSymbol) -> CompileResult<()> {
        
        match self.scopes_mut().insert_local(decl, LocalName::Symbol(name))? {
            InsertLocal::CreateNew => 
                self.emit_instr(symbol, OpCode::InsertLocal),
            
            InsertLocal::HideExisting(local_index) =>
                self.emit_assign_local(symbol, local_index),
        }
        
        Ok(())
    }
    
    fn compile_decl_global_name(&mut self, symbol: Option<&DebugSymbol>, decl: DeclType, name: InternSymbol) -> CompileResult<()> {
        
        self.emit_load_const(symbol, Constant::from(name))?;
        match decl {
            DeclType::Immutable => self.emit_instr(symbol, OpCode::InsertGlobal),
            DeclType::Mutable => self.emit_instr(symbol, OpCode::InsertGlobalMut),
        }
        Ok(())
    }
    
    fn compile_decl_tuple(&mut self, symbol: Option<&DebugSymbol>, decl: DeclType, targets: &[LValue]) -> CompileResult<()> {
        
        let mut error_jump_sites = Vec::new();
        
        self.emit_instr(symbol, OpCode::IterInit);  // iterate to yield values
        
        // compile to unrolled iteration
        for target in targets.iter() {
            // check if there is an item left for this target
            let error_jump = self.emit_dummy_jump(symbol, Jump::IfFalse);
            error_jump_sites.push(error_jump);
            
            // advance the iterator and put the item on the stack
            self.emit_instr(symbol, OpCode::IterNext);
            self.compile_declaration(symbol, decl, target)?;
            
            self.emit_instr(symbol, OpCode::Pop);
        }
        
        // if the iterator is finished we've succeeded
        let done_jump_site = self.emit_dummy_jump(symbol, Jump::PopIfFalse);
        
        // too many items
        self.emit_instr(symbol, OpCode::False);
        self.emit_instr(symbol, OpCode::Assert); // TODO implement dynamic errors
        
        // not enough items
        let error_target = self.current_offset();
        for jump_site in error_jump_sites.iter() {
            self.patch_jump_instr(jump_site, error_target)?;
        }
        
        self.emit_instr(symbol, OpCode::False);
        self.emit_instr(symbol, OpCode::Assert); // TODO implement dynamic errors
        
        // cleanup
        self.patch_jump_instr(&done_jump_site, self.current_offset())?;
        self.emit_instr(symbol, OpCode::Pop); // pop iter
        
        Ok(())
    }
    
    fn compile_update_assignment(&mut self, symbol: Option<&DebugSymbol>, op: BinaryOp, lhs: &LValue, rhs: &Expr, nonlocal: bool) -> CompileResult<()> {
        
        // TODO suport Attribute and Index LValues as well
        match lhs {
            LValue::Identifier(name) => {
                self.compile_name_lookup(symbol, name)?;
                self.compile_expr(symbol, rhs)?;
                self.emit_binary_op(symbol, &op);
                
                self.compile_assign_identifier(symbol, name, nonlocal)
            },
            
            LValue::Attribute(target) => unimplemented!(),
            LValue::Index(target) => unimplemented!(),
            
            LValue::Tuple(..) => Err(ErrorKind::CantUpdateAssignTuple.into()),
        }
    }
    
    fn compile_assignment(&mut self, symbol: Option<&DebugSymbol>, lhs: &LValue, nonlocal: bool) -> CompileResult<()> {
        
        match lhs {
            LValue::Identifier(name) => self.compile_assign_identifier(symbol, name, nonlocal),
            
            LValue::Attribute(target) => unimplemented!(),
            
            LValue::Index(target) => unimplemented!(),
            
            LValue::Tuple(target_list) => self.compile_assign_tuple(symbol, target_list, nonlocal),
        }
    }
    
    fn compile_assign_identifier(&mut self, symbol: Option<&DebugSymbol>, name: &InternSymbol, nonlocal: bool) -> CompileResult<()> {
        
        // Generate assignment
        
        if !self.scopes().is_global_scope() {
            
            let local_name = LocalName::Symbol(*name);
            
            // check if the name is found in the local scope...
            let result = self.scopes().resolve_local(&local_name);
            
            if let Some(local) = result.cloned() {
                if local.decl() != DeclType::Mutable {
                    return Err(CompileError::from(ErrorKind::CantAssignImmutable));
                }
                
                self.emit_assign_local(symbol, local.index());
                
                return Ok(());
            }
            
            // nonlocal keyword is not required in the global frame
            if !nonlocal && !self.scopes().is_global_frame() {
                return Err(CompileError::from(ErrorKind::CantAssignNonLocal));
            }
            
            // check if an upvalue is found or can be created...
            if !self.scopes().is_global_frame() {
                if let Some(upval) = self.scopes_mut().resolve_or_create_upval(&local_name)? {
                    if upval.decl() != DeclType::Mutable {
                        return Err(CompileError::from(ErrorKind::CantAssignImmutable));
                    }
                    
                    let index = upval.index();
                    if let Ok(index) = u8::try_from(index) {
                        self.emit_instr_byte(symbol, OpCode::StoreUpvalue, index);
                    } else {
                        self.emit_instr_data(symbol, OpCode::StoreUpvalue16, &index.to_le_bytes());
                    }
                    
                    return Ok(());
                }
            }
        }

        // ...finally, try to assign to a global, which are late bound
        self.emit_load_const(symbol, Constant::from(*name))?;
        self.emit_instr(symbol, OpCode::StoreGlobal);
        Ok(())
    }
    
    fn emit_assign_local(&mut self, symbol: Option<&DebugSymbol>, offset: LocalIndex) {
        if let Ok(offset) = u8::try_from(offset) {
            self.emit_instr_byte(symbol, OpCode::StoreLocal, offset);
        } else {
            self.emit_instr_data(symbol, OpCode::StoreLocal16, &offset.to_le_bytes());
        }
    }
    
    fn compile_assign_tuple(&mut self, symbol: Option<&DebugSymbol>, targets: &[LValue], nonlocal: bool) -> CompileResult<()> {
        
        let mut error_jump_sites = Vec::new();
        
        self.emit_instr(symbol, OpCode::IterInit);  // iterate to yield values
        
        // compile to unrolled iteration
        for target in targets.iter() {
            // check if there is an item left for this target
            let error_jump = self.emit_dummy_jump(symbol, Jump::IfFalse);
            error_jump_sites.push(error_jump);
            
            // advance the iterator and put the item on the stack
            self.emit_instr(symbol, OpCode::IterNext);
            self.compile_assignment(symbol, target, nonlocal)?;
            
            self.emit_instr(symbol, OpCode::Pop);
        }
        
        // if the iterator is finished we've succeeded
        let done_jump_site = self.emit_dummy_jump(symbol, Jump::PopIfFalse);
        
        // too many items
        self.emit_instr(symbol, OpCode::False);
        self.emit_instr(symbol, OpCode::Assert); // TODO implement dynamic errors
        
        // not enough items
        let error_target = self.current_offset();
        for jump_site in error_jump_sites.iter() {
            self.patch_jump_instr(jump_site, error_target)?;
        }
        
        self.emit_instr(symbol, OpCode::False);
        self.emit_instr(symbol, OpCode::Assert); // TODO implement dynamic errors
        
        // cleanup
        self.patch_jump_instr(&done_jump_site, self.current_offset())?;
        self.emit_instr(symbol, OpCode::Pop); // pop iter
        
        Ok(())
    }
}

///////// Blocks and If-Expressions /////////
impl CodeGenerator<'_> {

    fn compile_block_expression(&mut self, symbol: Option<&DebugSymbol>, label: Option<&Label>, suite: &ExprBlock) -> CompileResult<()> {
        
        self.emit_begin_scope(symbol, ScopeTag::Block, label);
        self.compile_expr_block(symbol, suite)?;
        let block_scope = self.emit_end_scope();
        
        // finalize scope
        let break_target = self.current_offset();
        self.finalize_scope(&block_scope, break_target)?;
        
        Ok(())
    }
    
    fn compile_if_expression(&mut self, symbol: Option<&DebugSymbol>, branches: &[ConditionalBranch], else_clause: Option<&ExprBlock>) -> CompileResult<()> {
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
            
            self.compile_expr(symbol, branch.condition())?;
            
            // need to keep condition value on the stack in case there is a break/continue
            // inside the statement list
            let branch_jump_site = self.emit_dummy_jump(symbol, Jump::IfFalse);
            
            self.emit_begin_scope(symbol, ScopeTag::Branch, None);
            self.compile_expr_block(symbol, branch.suite())?;
            self.emit_end_scope();
            
            // site for the jump to the end of if-expression
            if !is_final_branch {
                self.emit_instr(symbol, OpCode::Pop);
                let jump_site = self.emit_dummy_jump(symbol, Jump::Uncond);
                end_jump_sites.push(jump_site);
            }
            
            // target for the jump from the conditional of the now compiled branch
            self.patch_jump_instr(&branch_jump_site, self.current_offset())?;
        }
        
        // else clause
        if let Some(suite) = else_clause {
            
            self.emit_begin_scope(symbol, ScopeTag::Branch, None);
            self.compile_expr_block(symbol, suite)?;
            self.emit_end_scope();
            
        }
        
        // patch all of the end jump sites
        let end_target = self.current_offset();
        for jump_site in end_jump_sites.iter() {
            self.patch_jump_instr(jump_site, end_target)?;
        }
        
        Ok(())
    }
}

///////// Function Definitions /////////
impl CodeGenerator<'_> {
    fn compile_function_def(&mut self, symbol: Option<&DebugSymbol>, fundef: &FunctionDef) -> CompileResult<()> {
        // create a new chunk for the function
        let info = ChunkInfo::Function {
            symbol: symbol.copied(),
        };

        let mut chunk_gen = self.create_chunk(info)?;
        
        let chunk_id = chunk_gen.chunk_id();
        let fun_id = match chunk_id {
            Chunk::Function(fun_id) => fun_id,
            _ => panic!("chunk {:?} is not valid for function", chunk_id),
        };
        
        // and a new local scope
        // don't need to emit new scope instructions, should handled by function call
        chunk_gen.scopes_mut().push_frame(symbol);
        
        // don't need to generate IN_LOCAL instructions for these, the VM should include them automatically
        chunk_gen.scopes_mut().insert_local(DeclType::Immutable, LocalName::Receiver)?;
        chunk_gen.scopes_mut().insert_local(DeclType::Immutable, LocalName::NArgs)?;
        
        // prepare argument list
        chunk_gen.compile_function_preamble(symbol, fundef)?;
        
        // function body
        chunk_gen.compile_stmt_block(fundef.body.stmt_list())?;
        
        // function result
        if let Some(expr) = fundef.body.result() {
            chunk_gen.compile_expr_with_symbol(expr)?;
        } else {
            chunk_gen.emit_instr(symbol, OpCode::Nil);
        }
        
        // end the function scope
        // don't need to drop locals explicitly, that will be done when the VMCallFrame returns
        let frame = chunk_gen.scopes_mut().pop_frame();
        
        // however we do still need to close upvalues before we return
        for local in frame.iter_locals().filter(|local| local.captured()) {
            chunk_gen.emit_close_upvalue(None, local.index());
        }
        
        chunk_gen.finish();
        
        // compile the function signature
        let signature = self.compile_function_signature(symbol, &fundef.signature)?;
        
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
        self.emit_load_function(symbol, fun_id);
        
        Ok(())
    }
    
    fn compile_function_preamble(&mut self, symbol: Option<&DebugSymbol>, fundef: &FunctionDef) -> CompileResult<()> {
        
        // define locals
        let signature = &fundef.signature;
        
        for param in signature.required.iter() {
            self.scopes_mut().insert_local(param.decl, LocalName::Symbol(param.name))?;
            //self.emit_instr(None, OpCode::InsertLocal);
        }
        
        // will define local variables for all default arguments
        if !signature.default.is_empty() {
            self.compile_default_args(signature)?;
            
            for param in signature.default.iter() {
                self.scopes_mut().insert_local(param.decl, LocalName::Symbol(param.name))?;
                // self.emit_instr(symbol, OpCode::InsertLocal);
            }
        }
        
        if let Some(param) = &signature.variadic {
            self.compile_variadic_arg(signature)?;

            self.scopes_mut().insert_local(param.decl, LocalName::Symbol(param.name))?;
            // self.emit_instr(symbol, OpCode::InsertLocal);
        }

        Ok(())
    }
    
    fn compile_default_args(&mut self, signature: &SignatureDef) -> CompileResult<()> {
        debug_assert!(!signature.default.is_empty());
        
        let mut jump_sites = Vec::new();
        let mut jump_targets = Vec::new();
        
        // depending on the number of arguments, jump into the default argument sequence
        let required_count = u8::try_from(signature.required.len())
            .map_err(|_| ErrorKind::InternalLimit("parameter count limit exceeded"))?;
        let default_count = u8::try_from(signature.default.len())
            .map_err(|_| ErrorKind::InternalLimit("parameter count limit exceeded"))?;
        
        // "defaults passed" = NArgs - required_count
        self.try_emit_load_local(None, &LocalName::NArgs).unwrap();
        if required_count != 0 {
            self.emit_instr_byte(None, OpCode::UInt8, required_count);
            self.emit_instr(None, OpCode::Sub);
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
            self.emit_instr(None, OpCode::Clone);
            self.emit_instr_byte(None, OpCode::UInt8, count);
            self.emit_instr(None, OpCode::EQ);
            
            let jump_site = self.emit_dummy_jump(None, Jump::PopIfTrue);
            jump_sites.insert(count.into(), jump_site);
        }
        
        // if we get here, jump unconditionally to the end
        let jump_site = self.emit_dummy_jump(None, Jump::Uncond);
        jump_sites.insert(default_count.into(), jump_site);
        
        // generate default arguments
        for (idx, param) in signature.default.iter().enumerate() {
            jump_targets.insert(idx, self.current_offset());
            
            let symbol = Some(param.default.debug_symbol());
            let expr = param.default.variant();
            self.compile_expr(symbol, expr)?;
            self.emit_instr(None, OpCode::InsertLocal);
        }
        
        jump_targets.insert(default_count.into(), self.current_offset());
        self.emit_instr(None, OpCode::Pop);  // drop "defaults passed"
        
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
            .map_err(|_| ErrorKind::InternalLimit("parameter count limit exceeded"))?;
        
        // "variadic count" = NArgs - required_count - default_count
        self.try_emit_load_local(None, &LocalName::NArgs).unwrap();
        if positional_count != 0 {
            self.emit_instr_byte(None, OpCode::UInt8, positional_count);
            self.emit_instr(None, OpCode::Sub);
        }
        
        // check if "variadic count" > 0
        self.emit_instr(None, OpCode::Clone);
        self.emit_instr_byte(None, OpCode::UInt8, 0);
        self.emit_instr(None, OpCode::GT);
        
        let tuple_jump_site = self.emit_dummy_jump(None, Jump::PopIfTrue);
        
        // if we get here then the variadic arg is empty
        self.emit_instr(None, OpCode::Pop);
        self.emit_instr(None, OpCode::Empty);
        
        let end_jump_site = self.emit_dummy_jump(None, Jump::Uncond);
        
        self.patch_jump_instr(&tuple_jump_site, self.current_offset())?;
        self.emit_instr(None, OpCode::TupleN);
        self.emit_instr(None, OpCode::InsertLocal);
        
        self.patch_jump_instr(&end_jump_site, self.current_offset())?;
        
        Ok(())
    }
    
    fn compile_function_signature(&mut self, symbol: Option<&DebugSymbol>, signature: &SignatureDef) -> CompileResult<UnloadedSignature> {
        let name = 
            if let Some(name) = signature.name {
                Some(self.get_or_make_const(Constant::from(name))?)
            } else { None };
        
        let mut required = Vec::new();
        for param in signature.required.iter() {
            let name = self.get_or_make_const(Constant::from(param.name))?;
            required.push(UnloadedParam { name, decl: param.decl });
        }
        
        let mut default = Vec::new();
        for param in signature.default.iter() {
            let name = self.get_or_make_const(Constant::from(param.name))?;
            default.push(UnloadedParam { name, decl: param.decl });
        }
        
        let mut variadic = None;
        if let Some(param) = &signature.variadic {
            let name = self.get_or_make_const(Constant::from(param.name))?;
            variadic.replace(UnloadedParam { name, decl: param.decl });
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
