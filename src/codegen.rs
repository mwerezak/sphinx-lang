#![allow(unused_variables)]

use std::iter;
use crate::language::{IntType, FloatType, InternSymbol};
use crate::parser::stmt::{StmtMeta, Stmt, Label, StmtList, ControlFlow};
use crate::parser::expr::{Expr, ExprMeta, ExprBlock, ConditionalBranch};
use crate::parser::primary::{Atom, Primary, AccessItem};
use crate::parser::lvalue::{Assignment, Declaration, LValue, DeclType};
use crate::parser::fundefs::{FunctionDef, SignatureDef};
use crate::runtime::vm::LocalIndex;
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::strings::{StringInterner};
use crate::debug::symbol::{DebugSymbol, ChunkSymbols, DebugSymbolTable};

mod scope;

pub mod chunk;
pub mod consts;
pub mod opcodes;
pub mod errors;

pub use opcodes::OpCode;
pub use chunk::{UnloadedProgram, Program, ProgramData, Chunk, ChunkID};
pub use consts::{ConstID, Constant};
pub use errors::{CompileResult, CompileError, ErrorKind};

use scope::{ScopeTracker, LocalName, UpvalueTarget};
use chunk::{ChunkBuilder, ChunkInfo, ChunkBuf};
use consts::{UnloadedSignature, UnloadedParam};


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
        
        (Jump::Uncond,  JumpOffset::Long(..))  => OpCode::LongJump,
        (Jump::IfFalse, JumpOffset::Long(..))  => OpCode::LongJumpIfFalse,
        (Jump::IfTrue,  JumpOffset::Long(..))  => OpCode::LongJumpIfTrue,
        
        (Jump::PopIfFalse, JumpOffset::Short(..))  => OpCode::PopJumpIfFalse,
        (Jump::PopIfTrue,  JumpOffset::Short(..))  => OpCode::PopJumpIfTrue,
        
        (Jump::PopIfFalse, JumpOffset::Long(..))   => OpCode::PopLongJumpIfFalse,
        (Jump::PopIfTrue,  JumpOffset::Long(..))   => OpCode::PopLongJumpIfTrue,
    }
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
    scope: ScopeTracker,
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
            scope: ScopeTracker::new(),
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
            self.get_chunk(Chunk::Main).finish();
            
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
        self.emit_instr(None, OpCode::Return);
    }
    
    fn chunk_id(&self) -> Chunk { self.chunk_id }
    
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
    
    ///////// Emitting Bytecode /////////
    
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
    
    ///////// Patching Bytecode /////////
    
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
    
    ///////// Constants /////////
    
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
    
    fn emit_begin_scope(&mut self, symbol: Option<&DebugSymbol>) {
        let chunk_id = self.chunk_id;
        self.scope_mut().push_scope(symbol);
    }
    
    fn emit_end_scope(&mut self) {
        let scope = self.scope_mut().pop_scope();
        let symbol = scope.debug_symbol();
        
        // close all upvalues
        for local in scope.locals().iter().filter(|local| local.captured()) {
            self.emit_close_upvalue(symbol, local.index());
        }
        
        // discard all the locals from the stack
        let mut discard = scope.locals().len();
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
        if let Some(index) = self.scope().resolve_local(name).map(|local| local.index()) {
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
        if let Some(index) = self.scope_mut().resolve_or_create_upval(name)?.map(|upval| upval.index()) {
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
    
    fn emit_upvalue(&mut self, symbol: Option<&DebugSymbol>, target: UpvalueTarget) {
        match target {
            UpvalueTarget::Local(index) => {
                if let Ok(index) = u8::try_from(index) {
                    self.emit_instr_byte(symbol, OpCode::InsertUpvalueLocal, index);
                } else {
                    self.emit_instr_data(symbol, OpCode::InsertUpvalueLocal16, &index.to_le_bytes());
                }
            },
            
            UpvalueTarget::Upvalue(index) => {
                if let Ok(index) = u8::try_from(index) {
                    self.emit_instr_byte(symbol, OpCode::InsertUpvalueExtern, index);
                } else {
                    self.emit_instr_data(symbol, OpCode::InsertUpvalueExtern16, &index.to_le_bytes());
                }
            },
        }
    }
    
    fn emit_close_upvalue(&mut self, symbol: Option<&DebugSymbol>, index: LocalIndex) {
        if let Ok(index) = u8::try_from(index) {
            self.emit_instr_byte(symbol, OpCode::CloseUpvalue, index);
        } else {
            self.emit_instr_data(symbol, OpCode::CloseUpvalue16, &index.to_le_bytes());
        }
    }
    
    ///////// Statements /////////
    
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
        
        // handle control flow
        if let Some(control) = stmt_list.end_control() {
            match control {
                ControlFlow::Continue(label, symbol) => {
                    unimplemented!()
                }
                ControlFlow::Break(label, expr, symbol) => {
                    unimplemented!()
                }
                ControlFlow::Return(expr, symbol) => {
                    let symbol = symbol.as_ref();
                    if let Some(expr) = expr {
                        self.compile_expr(symbol, expr)?;
                    } else {
                        self.emit_instr(symbol, OpCode::Nil);
                    }
                    self.emit_instr(symbol, OpCode::Return);
                }
            }
        }
        
        Ok(())
    }
    
    fn compile_loop(&mut self, symbol: Option<&DebugSymbol>, label: Option<&Label>, body: &StmtList) -> CompileResult<()> {
        
        let loop_target = self.current_offset();
        
        self.emit_begin_scope(symbol);
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
        
        self.emit_begin_scope(symbol);
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
            
            Expr::Declaration(decl) => self.compile_declaration(symbol, (&**decl).into())?,
            Expr::Assignment(assign) => self.compile_assignment(symbol, (&**assign).into())?,
            
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
            .map_err(|_| CompileError::from(ErrorKind::TupleLengthLimit))?;
        
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
    
    fn compile_integer(&mut self, symbol: Option<&DebugSymbol>, value: IntType) -> CompileResult<()> {
        if let Ok(value) = u8::try_from(value) {
            self.emit_instr_byte(symbol, OpCode::UInt8, value);
        } else if let Ok(value) = i8::try_from(value) {
            self.emit_instr_byte(symbol, OpCode::Int8, value.to_le_bytes()[0]);
        } else {
            self.emit_load_const(symbol, Constant::from(value))?;
        }
        Ok(())
    }
    
    fn compile_float(&mut self, symbol: Option<&DebugSymbol>, value: FloatType) -> CompileResult<()> {
        if FloatType::from(i8::MIN) <= value && value <= FloatType::from(i8::MAX) {
            let value = value as i8;
            self.emit_instr_byte(symbol, OpCode::Float8, value.to_le_bytes()[0]);
        } else {
            self.emit_load_const(symbol, Constant::from(value))?;
        }
        Ok(())
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
                .map_err(|_| CompileError::from(ErrorKind::ArgCountLimit))?;
            
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
    
    ///////// Declarations and Assignments /////////
    
    fn compile_declaration(&mut self, symbol: Option<&DebugSymbol>, decl: DeclarationRef) -> CompileResult<()> {
        match &decl.lhs {
            LValue::Identifier(name) => if self.scope().is_global_scope() {
                self.compile_decl_global_name(symbol, decl.decl, *name, decl.init)
            } else {
                self.compile_decl_local_name(symbol, decl.decl, *name, decl.init)
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
        
        self.scope_mut().insert_local(decl, LocalName::Symbol(name))?;
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
                Err(CompileError::new("can't use update-assigment when assigning to a tuple"))
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
            self.compile_expr(symbol, assign.rhs)?;
            self.emit_binary_op(symbol, &op);
            
        } else {
            // normal assignment
            self.compile_expr(symbol, assign.rhs)?;
            
        }
        
        // Generate assignment
        
        if !self.scope().is_global_scope() {
            
            let local_name = LocalName::Symbol(*name);
            
            // check if the name is found in the local scope...
            let result = self.scope().resolve_local(&local_name);
            
            if let Some(local) = result.cloned() {
                if local.decl() != DeclType::Mutable {
                    return Err(CompileError::from(ErrorKind::CantAssignImmutable));
                }
                
                self.emit_assign_local(symbol, local.index());
                
                return Ok(());
            }
            
            // nonlocal keyword is not required in the global frame
            if !assign.nonlocal && !self.scope().is_global_frame() {
                return Err(CompileError::from(ErrorKind::CantAssignNonLocal));
            }
            
            // check if an upvalue is found or can be created...
            if !self.scope().is_global_frame() {
                if let Some(upval) = self.scope_mut().resolve_or_create_upval(&local_name)? {
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
    
    
    ///////// Blocks and If-Expressions /////////
    
    fn compile_expr_block(&mut self, symbol: Option<&DebugSymbol>, suite: &ExprBlock) -> CompileResult<()> {
        self.compile_stmt_list(suite.stmt_list())?;
        
        // result expression
        if let Some(expr) = suite.result() {
            self.compile_expr_with_symbol(expr)?;
        } else {
            self.emit_instr(symbol, OpCode::Nil); // implicit nil
        }
        
        Ok(())
    }
    
    fn compile_block_expression(&mut self, symbol: Option<&DebugSymbol>, label: Option<&Label>, suite: &ExprBlock) -> CompileResult<()> {
        
        self.emit_begin_scope(symbol);
        self.compile_expr_block(symbol, suite)?;
        self.emit_end_scope();
        
        Ok(())
    }
    
    fn compile_if_expression(&mut self, symbol: Option<&DebugSymbol>, branches: &[ConditionalBranch], else_clause: Option<&ExprBlock>) -> CompileResult<()> {
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
            
            self.emit_begin_scope(symbol);
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
            
            self.emit_begin_scope(symbol);
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
    
    
    ///////// Function Definitions /////////
    
    fn compile_function_def(&mut self, symbol: Option<&DebugSymbol>, fundef: &FunctionDef) -> CompileResult<()> {
        // compile the function signature
        let signature = self.compile_function_signature(symbol, &fundef.signature)?;
        let function_id = self.builder_mut().push_function(signature);
        
        // create a new chunk for the function
        let info = ChunkInfo::Function {
            id: function_id,
            symbol: symbol.map(|symbol| *symbol),
        };

        let mut chunk_gen = self.create_chunk(info)?;
        
        let chunk = chunk_gen.chunk_id();
        let chunk_id = match chunk {
            Chunk::ChunkID(chunk_id) => chunk_id,
            _ => panic!("chunk {:?} is not valid for function", chunk),
        };
        
        // and a new local scope
        // don't need to emit new scope instructions, should handled by function call
        chunk_gen.scope_mut().push_frame(symbol);
        
        // don't need to generate IN_LOCAL instructions for these, the VM should include them automatically
        chunk_gen.scope_mut().insert_local(DeclType::Immutable, LocalName::Receiver)?;
        chunk_gen.scope_mut().insert_local(DeclType::Immutable, LocalName::NArgs)?;
        
        // prepare argument list
        chunk_gen.compile_function_preamble(symbol, fundef)?;
        
        // function body
        chunk_gen.compile_stmt_list(fundef.body.stmt_list())?;
        
        // function result
        if let Some(expr) = fundef.body.result() {
            chunk_gen.compile_expr_with_symbol(expr)?;
        } else {
            chunk_gen.emit_instr(symbol, OpCode::Nil);
        }
        
        // end the function scope
        // don't need to drop locals explicitly, that will be done when the VMCallFrame returns
        let frame = chunk_gen.scope_mut().pop_frame();
        
        // however we do still need to close upvalues before we return
        for local in frame.iter_locals().filter(|local| local.captured()) {
            chunk_gen.emit_close_upvalue(None, local.index());
        }
        
        chunk_gen.finish();
        
        // load the function object as the expression result
        self.emit_load_const(symbol, Constant::Function(chunk_id, function_id))?;
        
        // compile upvalues
        for upvalue in frame.upvalues().iter() {
            self.emit_upvalue(symbol, upvalue.target());
        }
        
        Ok(())
    }
    
    fn compile_function_preamble(&mut self, symbol: Option<&DebugSymbol>, fundef: &FunctionDef) -> CompileResult<()> {
        
        // define locals
        let signature = &fundef.signature;
        
        for param in signature.required.iter() {
            self.scope_mut().insert_local(param.decl, LocalName::Symbol(param.name))?;
            //self.emit_instr(None, OpCode::InsertLocal);
        }
        
        // will define local variables for all default arguments
        if !signature.default.is_empty() {
            self.compile_default_args(signature)?;
            
            for param in signature.default.iter() {
                self.scope_mut().insert_local(param.decl, LocalName::Symbol(param.name))?;
                // self.emit_instr(symbol, OpCode::InsertLocal);
            }
        }
        
        if let Some(param) = &signature.variadic {
            self.compile_variadic_arg(signature)?;

            self.scope_mut().insert_local(param.decl, LocalName::Symbol(param.name))?;
            // self.emit_instr(symbol, OpCode::InsertLocal);
        }

        Ok(())
    }
    
    fn compile_default_args(&mut self, signature: &SignatureDef) -> CompileResult<()> {
        debug_assert!(!signature.default.is_empty());
        
        let mut jump_sites = Vec::new();
        let mut jump_targets = Vec::new();
        let mut jump_types = Vec::new();
        
        // depending on the number of arguments, jump into the default argument sequence
        let required_count = u8::try_from(signature.required.len())
            .map_err(|_| CompileError::from(ErrorKind::ParamCountLimit))?;
        let default_count = u8::try_from(signature.default.len())
            .map_err(|_| CompileError::from(ErrorKind::ParamCountLimit))?;
        
        // "defaults passed" = NArgs - required_count
        self.try_emit_load_local(None, &LocalName::NArgs).unwrap();
        self.emit_instr_byte(None, OpCode::UInt8, required_count);
        self.emit_instr(None, OpCode::Sub);
        
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
            
            jump_sites.insert(count.into(), self.current_offset());
            jump_types.insert(count.into(), Jump::PopIfTrue);
            self.emit_dummy_instr(None, Jump::PopIfTrue.dummy_width());
            
        }
        
        // if we get here, jump unconditionally to the end
        jump_sites.insert(default_count.into(), self.current_offset());
        jump_types.insert(default_count.into(), Jump::Uncond);
        self.emit_dummy_instr(None, Jump::Uncond.dummy_width());
        
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
        for idx in 0..=default_count.into() {
            self.patch_jump_instr(jump_types[idx], jump_sites[idx], jump_types[idx].dummy_width(), jump_targets[idx])?;
        }
        
        Ok(())
    }
    
    fn compile_variadic_arg(&mut self, signature: &SignatureDef) -> CompileResult<()> {
        debug_assert!(signature.variadic.is_some());
        
        let positional_count = u8::try_from(signature.required.len() + signature.default.len())
            .map_err(|_| CompileError::from(ErrorKind::ParamCountLimit))?;
        
        // "variadic count" = NArgs - required_count - default_count
        self.try_emit_load_local(None, &LocalName::NArgs).unwrap();
        self.emit_instr_byte(None, OpCode::UInt8, positional_count);
        self.emit_instr(None, OpCode::Sub);
        
        // check if "variadic count" > 0
        self.emit_instr(None, OpCode::Clone);
        self.emit_instr_byte(None, OpCode::UInt8, 0);
        self.emit_instr(None, OpCode::GT);
        
        let tuple_jump_site = self.current_offset();
        self.emit_dummy_instr(None, Jump::PopIfTrue.dummy_width());
        
        // if we get here then the variadic arg is empty
        self.emit_instr(None, OpCode::Pop);
        self.emit_instr(None, OpCode::Empty);
        
        let end_jump_site = self.current_offset();
        self.emit_dummy_instr(None, Jump::Uncond.dummy_width());
        
        let tuple_jump_target = self.current_offset();
        self.patch_jump_instr(Jump::PopIfTrue, tuple_jump_site, Jump::PopIfTrue.dummy_width(), tuple_jump_target)?;
        self.emit_instr(None, OpCode::TupleN);
        self.emit_instr(None, OpCode::InsertLocal);
        
        let end_jump_target = self.current_offset();
        self.patch_jump_instr(Jump::Uncond, end_jump_site, Jump::Uncond.dummy_width(), end_jump_target)?;
        
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
