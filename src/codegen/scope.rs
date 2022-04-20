// Scope Tracking

use crate::language::InternSymbol;
use crate::parser::lvalue::DeclType;
use crate::debug::symbol::DebugSymbol;
use crate::codegen::opcodes::{LocalIndex, UpvalueIndex};
use crate::codegen::funproto::UpvalueTarget;
use crate::codegen::errors::{CompileResult, ErrorKind};


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
    captured: bool, // tracks whether the local is being referenced by an upvalue
}

impl Local {
    pub fn decl(&self) -> DeclType { self.decl }
    pub fn name(&self) -> LocalName { self.name }
    pub fn index(&self) -> LocalIndex { self.index }
    pub fn captured(&self) -> bool { self.captured }
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
                .ok_or_else(|| ErrorKind::InternalLimit("local variable limit reached"))
        )?;
        
        let local = Local {
            decl, name, index, 
            captured: false,
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
    
    /// Iterate in name resolution order
    fn iter(&self) -> impl Iterator<Item=&Scope> {
        self.scopes.iter().rev()
    }
    
    fn iter_mut(&mut self) -> impl Iterator<Item=&mut Scope> {
        self.scopes.iter_mut().rev()
    }
}


#[derive(Debug, Clone)]
pub struct Upvalue {
    decl: DeclType,
    name: LocalName,
    index: UpvalueIndex,
    target: UpvalueTarget,
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
    
    pub fn iter_locals(&self) -> impl Iterator<Item=&Local> {
        self.scopes().iter().flat_map(|scope| scope.locals().iter())
    }
    
    fn scopes(&self) -> &NestedScopes { &self.scopes }
    
    fn scopes_mut(&mut self) -> &mut NestedScopes { &mut self.scopes }
    
    fn find_upval(&self, name: &LocalName) -> Option<&Upvalue> {
        self.upvalues.iter().find(|upval| upval.name == *name)
    }
    
    fn create_upval_for_local(&mut self, local: &mut Local) -> CompileResult<&Upvalue> {
        let index = UpvalueIndex::try_from(self.upvalues.len())
            .map_err(|_| ErrorKind::InternalLimit("upvalue limit reached"))?;
        
        let upval = Upvalue {
            index,
            decl: local.decl,
            name: local.name,
            target: UpvalueTarget::Local(local.index),
        };
        self.upvalues.push(upval);
        
        local.captured = true;
        
        Ok(self.upvalues.last().unwrap())
    }
    
    fn create_upval_for_upval(&mut self, upval: &Upvalue) -> CompileResult<&Upvalue> {
        let index = UpvalueIndex::try_from(self.upvalues.len())
            .map_err(|_| ErrorKind::InternalLimit("upvalue limit reached"))?;
        
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
    
    pub fn is_global_scope(&self) -> bool {
        self.frames.is_empty() && self.scopes.is_empty()
    }
    
    pub fn is_global_frame(&self) -> bool {
        self.frames.is_empty()
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
    
    pub fn resolve_local(&self, name: &LocalName) -> Option<&Local> {
        self.current_scope()
            .iter().find_map(|scope| scope.find_local(name))
    }
    
    pub fn resolve_or_create_upval(&mut self, name: &LocalName) -> CompileResult<Option<&Upvalue>> {
        if self.frames.is_empty() {
            return Ok(None);
        }
        
        let frame_idx = self.frames.len() - 1;
        let upval = self.resolve_upval_helper(name, frame_idx)?
            .map(|idx| &self.frames.last().unwrap().upvalues[usize::from(idx)]);
        
        Ok(upval)
    }
    
    // recursive helper
    fn resolve_upval_helper(&mut self, name: &LocalName, frame_idx: usize) -> CompileResult<Option<UpvalueIndex>> {
        {
            let (current_frame, enclosing_frame) = Self::get_frames_mut(&mut self.frames, frame_idx);
            
            // check if the upvalue already exists in the current frame
            if let Some(upval) = current_frame.find_upval(name) {
                return Ok(Some(upval.index));
            }
            
            // check if the local name exists in the enclosing scope
            let enclosing = enclosing_frame.map_or(&mut self.scopes, |frame| frame.scopes_mut());
            if let Some(local) = enclosing.iter_mut().find_map(|scope| scope.find_local_mut(name)) {
                return Ok(Some(current_frame.create_upval_for_local(local)?.index));
            }
        }
        
        // check if an upvalue can be created in the enclosing scope to a local further down
        if frame_idx > 0 {
            if let Some(upval_idx) = self.resolve_upval_helper(name, frame_idx-1)? {
                let (current_frame, enclosing_frame) = Self::get_frames_mut(&mut self.frames, frame_idx);
                if let Some(enclosing_frame) = enclosing_frame {
                    let upval = &enclosing_frame.upvalues()[usize::from(upval_idx)];
                    
                    return Ok(Some(current_frame.create_upval_for_upval(upval)?.index));
                }
            }
        }
        
        Ok(None)
    }
    
    // helper to get a frame by index and its enclosing frame
    fn get_frames_mut(frames: &mut [ScopeFrame], frame_idx: usize) -> (&mut ScopeFrame, Option<&mut ScopeFrame>) {
        let (frames, _) = frames.split_at_mut(frame_idx + 1);
        let (current_frame, frames) = frames.split_last_mut().unwrap();
        let enclosing_frame = frames.split_last_mut().map(|(last, _)| last);
        (current_frame, enclosing_frame)
    }
}