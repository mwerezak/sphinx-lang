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
    depth: usize,
    symbol: Option<DebugSymbol>,
    frame: Option<LocalIndex>, // last local index of the enclosing scope
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
        self.locals.last().map_or(self.frame, |local| Some(local.index))
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
    
    fn is_call_frame(&self) -> bool {
        self.frame.is_none()
    }
}

#[derive(Debug)]
pub struct ScopeTracker {
    scopes: Vec<Scope>,
}

impl ScopeTracker {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }
    
    pub fn is_global(&self) -> bool {
        self.scopes.is_empty()
    }
    
    pub fn local_scope(&self) -> Option<&Scope> {
        self.scopes.last()
    }
    
    pub fn local_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
    }
    
    pub fn push_frame(&mut self, symbol: Option<&DebugSymbol>) {
        let scope = Scope {
            frame: None,
            symbol: symbol.copied(),
            depth: self.scopes.len(),
            locals: Vec::new(),
        };
        
        self.scopes.push(scope);
    }
    
    /// Non-frame scope. e.g. block-expressions, if-blocks, loop bodies, etc.
    pub fn push_scope(&mut self, symbol: Option<&DebugSymbol>) {
        let frame = self.local_scope().and_then(|scope| scope.last_index());
        
        let scope = Scope {
            frame,
            symbol: symbol.copied(),
            depth: self.scopes.len(),
            locals: Vec::new(),
        };
        
        self.scopes.push(scope);
    }
    
    pub fn pop_scope(&mut self) -> Scope {
        self.scopes.pop().expect("pop global scope")
    }
    
    pub fn insert_local(&mut self, decl: DeclType, name: LocalName) -> CompileResult<()> {
        let local_scope = self.local_scope_mut().expect("insert local in global scope");
        
        // see if this local already exists in the current scope
        if let Some(mut local) = local_scope.find_local_mut(&name) {
            (*local).decl = decl; // redeclare with new mutability
        } else {
            local_scope.push_local(decl, name)?;
        }
        Ok(())
    }
    
    pub fn iter_scopes(&self) -> impl Iterator<Item=&Scope> {
        self.scopes.iter().rev()
    }
    
    // find the nearest local in scopes that allow nonlocal assignment
    pub fn resolve_local(&self, name: &LocalName) -> Option<&Local> {
        for scope in self.iter_scopes() {
            let local = scope.find_local(name);
            if local.is_some() {
                return local;
            }
            if scope.is_call_frame() {
                break;
            }
        }
        None
    }
    
    // without the nonlocal keyword, that is
    pub fn can_assign_global(&self) -> bool {
        self.scopes.iter().skip(1).all(|scope| !scope.is_call_frame())
    }
}