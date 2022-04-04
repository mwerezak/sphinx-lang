// Scope Tracking

use crate::language::{IntType, FloatType, InternSymbol};
use crate::parser::stmt::{StmtMeta, Stmt, Label, StmtList, ControlFlow};
use crate::parser::expr::{Expr, ExprMeta, ExprBlock, ConditionalBranch};
use crate::parser::primary::{Atom, Primary, AccessItem};
use crate::parser::lvalue::{Assignment, Declaration, LValue, DeclType};
use crate::parser::fundefs::{FunctionDef, SignatureDef, ParamDef, DefaultDef};
use crate::runtime::vm::LocalIndex;
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::function::UpvalueIndex;
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


#[derive(Debug)]
struct NestedScopes {
    scopes: Vec<Scope>,
}

impl NestedScopes {
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
    
    fn iter(&self) -> impl Iterator<Item=&Scope> {
        self.scopes.iter().rev()
    }
}


#[derive(Debug, Clone)]
pub struct Upvalue {
    decl: DeclType,
    name: LocalName,
    index: UpvalueIndex,
    target: UpvalueTarget,
}

#[derive(Debug, Clone, Copy)]
pub enum UpvalueTarget {
    Local(LocalIndex),
    Upvalue(UpvalueIndex),
}

impl Upvalue {
    pub fn decl(&self) -> DeclType { self.decl }
    pub fn name(&self) -> LocalName { self.name }
    pub fn index(&self) -> UpvalueIndex { self.index }
    pub fn target(&self) -> UpvalueTarget { self.target }
}


#[derive(Debug)]
pub struct ScopeFrame {
    scopes: NestedScopes,
    upvalues: Vec<Upvalue>,
}

impl ScopeFrame {
    fn new(symbol: Option<&DebugSymbol>) -> Self {
        let mut scopes = NestedScopes::new();
        scopes.push_scope(symbol);
        
        Self {
            scopes,
            upvalues: Vec::new(),
        }
    }
    
    pub fn upvalues(&self) -> &[Upvalue] { self.upvalues.as_slice() }
    
    fn scopes(&self) -> &NestedScopes { &self.scopes }
    
    fn scopes_mut(&mut self) -> &mut NestedScopes { &mut self.scopes }
    
    fn find_upval(&self, name: &LocalName) -> Option<&Upvalue> {
        self.upvalues.iter().find(|upval| upval.name == *name)
    }
    
    fn create_upval_for_local(&mut self, local: &Local) -> CompileResult<&Upvalue> {
        let index = UpvalueIndex::try_from(self.upvalues.len())
            .map_err(|_| CompileError::from(ErrorKind::UpvalueLimit))?;
        
        let upval = Upvalue {
            index,
            decl: local.decl,
            name: local.name,
            target: UpvalueTarget::Local(local.index),
        };
        
        self.upvalues.push(upval);
        
        Ok(self.upvalues.last().unwrap())
    }
    
    fn create_upval_for_upval(&mut self, upval: &Upvalue) -> CompileResult<&Upvalue> {
        let index = UpvalueIndex::try_from(self.upvalues.len())
            .map_err(|_| CompileError::from(ErrorKind::UpvalueLimit))?;
        
        let upval = Upvalue {
            index,
            decl: upval.decl,
            name: upval.name,
            target: UpvalueTarget::Upvalue(upval.index),
        };
        
        self.upvalues.push(upval);
        
        Ok(self.upvalues.last().unwrap())
    }
}


#[derive(Debug)]
pub struct ScopeTracker {
    scopes: NestedScopes,
    frames: Vec<ScopeFrame>,
}

impl ScopeTracker {
    pub fn new() -> Self {
        Self {
            scopes: NestedScopes::new(),
            frames: Vec::new(),
        }
    }
    
    pub fn is_global(&self) -> bool {
        self.frames.is_empty() && self.scopes.is_empty()
    }
    
    pub fn push_frame(&mut self, symbol: Option<&DebugSymbol>) {
        self.frames.push(ScopeFrame::new(symbol))
    }
    
    pub fn pop_frame(&mut self) -> ScopeFrame {
        self.frames.pop().expect("pop empty frames")
    }
    
    fn current_scope(&self) -> &NestedScopes {
        self.frames.last()
            .map_or(&self.scopes, |frame| frame.scopes())
    }
    
    fn current_scope_mut(&mut self) -> &mut NestedScopes {
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
        self.current_scope()
            .iter().find_map(|scope| scope.find_local(name))
    }
    
    pub fn resolve_or_create_upval(&mut self, name: &LocalName) -> CompileResult<Option<Upvalue>> {
        if self.frames.is_empty() {
            return Ok(None);
        }
        
        let current_idx = self.frames.len() - 1;
        self.resolve_upval_helper(name, current_idx)
    }
    
    fn resolve_upval_helper(&mut self, name: &LocalName, frame_idx: usize) -> CompileResult<Option<Upvalue>> {
        {
            let (frames, _) = self.frames.split_at_mut(frame_idx + 1);
            let (current_frame, frames) = frames.split_last_mut().unwrap();
            let enclosing_frame = frames.split_last_mut().map(|(last, _)| last);
            
            // check if the upvalue already exists in the current frame
            let upval = current_frame.find_upval(name);
            if upval.is_some() {
                return Ok(upval.cloned());
            }
            
            // check if the local name exists in the enclosing scope
            let enclosing = enclosing_frame.map_or(&self.scopes, |frame| frame.scopes());
            if let Some(local) = enclosing.iter().find_map(|scope| scope.find_local(name)) {
                return Ok(Some(current_frame.create_upval_for_local(local)?.clone()));
            }
        }
        
        // check if an upvalue can be created in the enclosing scope to a local further down
        if frame_idx > 0 {
            if let Some(upval) = self.resolve_upval_helper(name, frame_idx-1)? {
                let current_frame = &mut self.frames[frame_idx];
                return Ok(Some(current_frame.create_upval_for_upval(&upval)?.clone()));
            }
        }
        
        Ok(None)
    }
    
    
    // without the nonlocal keyword, that is
    pub fn can_assign_nonlocal(&self) -> bool {
        unimplemented!()
    }
}