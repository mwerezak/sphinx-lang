// Scope Tracking

use crate::language::{IntType, FloatType, InternSymbol};
use crate::parser::stmt::{StmtMeta, Stmt, Label, StmtList, ControlFlow};
use crate::parser::expr::{Expr, ExprMeta, ExprBlock, ConditionalBranch};
use crate::parser::primary::{Atom, Primary, AccessItem};
use crate::parser::lvalue::{Assignment, Declaration, LValue, DeclType};
use crate::parser::fundefs::{FunctionDef, SignatureDef, ParamDef, DefaultDef};
use crate::runtime::vm::LocalIndex;
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::strings::{StringInterner};
use crate::debug::symbol::{DebugSymbol, ChunkSymbols, DebugSymbolTable};
use crate::codegen::chunk::Chunk;
use crate::codegen::errors::{CompileResult, CompileError, ErrorKind};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LocalName {
    // local variable names defined by AST string symbols
    Symbol(InternSymbol),
    
    // special local variables
    Receiver,  // inside a function call, this refers to the object that was called
    NArgs,     // inside a function call, the number of arguments passed at the call site
}


#[derive(Debug, Clone)]
pub struct Local {
    decl: DeclType,
    name: LocalName,
    index: LocalIndex,
}

impl Local {
    pub fn decl(&self) -> DeclType { self.decl }
    pub fn name(&self) -> LocalName { self.name }
    pub fn index(&self) -> LocalIndex { self.index }
}


#[derive(Debug)]
pub struct Scope {
    symbol: Option<DebugSymbol>,
    prev_index: Option<LocalIndex>,
    locals: Vec<Local>,
}

impl Scope {
    pub fn locals(&self) -> &[Local] {
        self.locals.as_slice()
    }
    
    pub fn debug_symbol(&self) -> Option<&DebugSymbol> {
        self.symbol.as_ref()
    }
    
    fn last_index(&self) -> Option<LocalIndex> {
        self.locals.last().map_or(self.prev_index, |local| Some(local.index))
    }
    
    fn find_local(&self, name: &LocalName) -> Option<&Local> {
        self.locals.iter().find(|local| local.name == *name)
    }
    
    fn find_local_mut(&mut self, name: &LocalName) -> Option<&mut Local> {
        self.locals.iter_mut().find(|local| local.name == *name)
    }
    
    fn push_local(&mut self, decl: DeclType, name: LocalName) -> CompileResult<&Local> {
        let index = self.last_index().map_or(
            Ok(0),
            |index| index.checked_add(1)
                .ok_or_else(|| CompileError::from(ErrorKind::LocalVariableLimit))
        )?;
        
        let local = Local {
            decl, name, index,
        };
        self.locals.push(local);
        Ok(self.locals.last().unwrap())
    }
    
    fn insert_local(&mut self, decl: DeclType, name: LocalName) -> CompileResult<()> {
        // see if this local already exists in the current scope
        if let Some(mut local) = self.find_local_mut(&name) {
            (*local).decl = decl; // redeclare with new mutability
        } else {
            self.push_local(decl, name)?;
        }
        Ok(())
    }
}


// TODO: better name
#[derive(Debug)]
struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    fn new() -> Self {
        Self { scopes: Vec::new() }
    }
    
    fn is_empty(&self) -> bool {
        self.scopes.is_empty()
    }
    
    fn current_scope(&self) -> Option<&Scope> {
        self.scopes.last()
    }
    
    fn current_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
    }
    
    fn push_scope(&mut self, symbol: Option<&DebugSymbol>) {
        let prev_index = self.scopes.last().and_then(|scope| scope.last_index());
        
        let scope = Scope {
            prev_index,
            symbol: symbol.copied(),
            locals: Vec::new(),
        };
        
        self.scopes.push(scope);
    }
    
    fn pop_scope(&mut self) -> Scope {
        self.scopes.pop().expect("pop empty scope")
    }
    
    // find the nearest local in scopes that allow nonlocal assignment
    fn resolve_local(&self, name: &LocalName) -> Option<&Local> {
        self.scopes.iter().rev()
            .find_map(|scope| scope.find_local(name))
    }
}


#[derive(Debug)]
pub struct Upvalue {
    
}

#[derive(Debug)]
struct CallFrame {
    scopes: Scopes,
    upvalues: Vec<Upvalue>,
}

impl CallFrame {
    fn new(symbol: Option<&DebugSymbol>) -> Self {
        let mut scopes = Scopes::new();
        scopes.push_scope(symbol);
        
        Self {
            scopes,
            upvalues: Vec::new(),
        }
    }
    
    fn scopes(&self) -> &Scopes { &self.scopes }
    
    fn scopes_mut(&mut self) -> &mut Scopes { &mut self.scopes }
}


#[derive(Debug)]
pub struct ScopeTracker {
    scopes: Scopes,
    frames: Vec<CallFrame>,
}

impl ScopeTracker {
    pub fn new() -> Self {
        Self {
            scopes: Scopes::new(),
            frames: Vec::new(),
        }
    }
    
    pub fn is_global(&self) -> bool {
        self.frames.is_empty() && self.scopes.is_empty()
    }
    
    fn current_frame(&self) -> Option<&CallFrame> {
        self.frames.last()
    }
    
    pub fn push_frame(&mut self, symbol: Option<&DebugSymbol>) {
        self.frames.push(CallFrame::new(symbol))
    }
    
    pub fn pop_frame(&mut self) {
        self.frames.pop().expect("pop empty frames");
    }
    
    fn current_scope(&self) -> &Scopes {
        self.frames.last()
            .map_or(&self.scopes, |frame| frame.scopes())
    }
    
    fn current_scope_mut(&mut self) -> &mut Scopes {
        self.frames.last_mut()
            .map_or(&mut self.scopes, |frame| frame.scopes_mut())
    }
    
    pub fn local_scope(&self) -> Option<&Scope> {
        self.current_scope().current_scope()
    }
    
    pub fn push_scope(&mut self, symbol: Option<&DebugSymbol>) {
        self.current_scope_mut().push_scope(symbol);
    }
    
    pub fn pop_scope(&mut self) -> Scope {
        let scope = self.current_scope_mut().pop_scope();
        debug_assert!(self.frames.last().map_or(true, |frame| !frame.scopes().is_empty()), "pop last scope from call frame");
        scope
    }
    
    pub fn insert_local(&mut self, decl: DeclType, name: LocalName) -> CompileResult<()> {
        let scope = self.current_scope_mut().current_scope_mut().expect("insert local in global scope");
        scope.insert_local(decl, name)
    }
    
    // find the nearest local in scopes that allow nonlocal assignment
    pub fn resolve_local(&self, name: &LocalName) -> Option<&Local> {
        self.current_scope().resolve_local(name)
    }
    
    // without the nonlocal keyword, that is
    pub fn can_assign_nonlocal(&self) -> bool {
        unimplemented!()
    }
}