// Scope Tracking

use crate::language::{InternSymbol, Access};
use crate::parser::stmt::Label;
use crate::debug::symbol::DebugSymbol;
use crate::codegen::JumpSite;
use crate::codegen::opcodes::{LocalIndex, UpvalueIndex};
use crate::codegen::funproto::UpvalueTarget;
use crate::codegen::errors::CompileResult;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum LocalName {
    // local variable names defined by AST string symbols
    Symbol(InternSymbol),
    
    // special local variables
    Receiver,  // inside a function call, this refers to the object that was called
    NArgs,     // inside a function call, the number of arguments passed at the call site
    
    Anonymous,      // anonymous temporaries
}


#[derive(Debug, Clone)]
pub(super) struct Local {
    mode: Access,
    name: LocalName,
    index: LocalIndex,
    captured: bool, // tracks whether the local is being referenced by an upvalue
}

impl Local {
    pub(super) fn mode(&self) -> Access { self.mode }
    pub(super) fn name(&self) -> LocalName { self.name }
    pub(super) fn index(&self) -> LocalIndex { self.index }
    pub(super) fn captured(&self) -> bool { self.captured }
}

#[derive(Clone, Copy)]
pub(super) enum InsertLocal {
    CreateNew(LocalIndex),
    HideExisting(LocalIndex),
}

impl From<InsertLocal> for LocalIndex {
    fn from(result: InsertLocal) -> Self {
        match result {
            InsertLocal::CreateNew(local_index) => local_index,
            InsertLocal::HideExisting(local_index) => local_index,
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum ScopeTag {
    Block,
    Loop,
    Branch,
    Function,
    Global,
}

impl ScopeTag {
    fn accepts_control_flow(&self, control_flow: ControlFlowTarget) -> bool {
        match self {
            Self::Block => matches!(control_flow,
                ControlFlowTarget::Break(..)
            ),
            
            Self::Loop => matches!(control_flow,
                ControlFlowTarget::Break(..) | ControlFlowTarget::Continue(..)
            ),
            
            _ => false,
        }
    }
    
    pub(super) fn is_expr_block(&self) -> bool {
        match self {
            Self::Block | Self:: Branch => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum ControlFlowTarget {
    Break(Option<Label>),
    Continue(Option<Label>),
}

impl ControlFlowTarget {
    pub(super) fn label(&self) -> Option<&Label> {
        match self {
            Self::Break(label) => label.as_ref(),
            Self::Continue(label) => label.as_ref(),
        }
    }
}

// track break/continue jump sites
#[derive(Debug, Default)]
struct ControlFlowTracker {
    label: Option<Label>,
    continue_sites: Vec<JumpSite>,
    break_sites: Vec<JumpSite>,
}

impl ControlFlowTracker {
    fn new(label: Option<Label>) -> Self {
        Self {
            label,
            continue_sites: Vec::new(),
            break_sites: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub(super) struct Scope {
    tag: ScopeTag,
    depth: usize,
    symbol: Option<DebugSymbol>,
    prev_index: Option<LocalIndex>,
    locals: Vec<Local>,
    control_flow: ControlFlowTracker,
}

impl Scope {
    pub(super) fn tag(&self) -> ScopeTag {
        self.tag
    }
    
    pub(super) fn depth(&self) -> usize {
        self.depth
    }
    
    pub(super) fn locals(&self) -> &[Local] {
        self.locals.as_slice()
    }
    
    pub(super) fn debug_symbol(&self) -> Option<&DebugSymbol> {
        self.symbol.as_ref()
    }
    
    pub(super) fn register_continue(&mut self, continue_site: JumpSite) {
        self.control_flow.continue_sites.push(continue_site)
    }
    
    pub(super) fn continue_sites(&self) -> &[JumpSite] {
        &self.control_flow.continue_sites
    }
    
    pub(super) fn register_break(&mut self, break_site: JumpSite) {
        self.control_flow.break_sites.push(break_site)
    }
    
    pub(super) fn break_sites(&self) -> &[JumpSite] {
        &self.control_flow.break_sites
    }
    
    fn control_flow_mut(&mut self) -> &mut ControlFlowTracker {
        &mut self.control_flow
    }
    
    fn last_index(&self) -> Option<LocalIndex> {
        self.locals.last().map_or(self.prev_index, |local| Some(local.index))
    }
    
    fn find_local(&self, name: &LocalName) -> Option<&Local> {
        if matches!(name, LocalName::Anonymous) {
            return None; // anonymous temporaries should not be referenced
        }
        self.locals.iter().find(|local| local.name == *name)
    }
    
    fn find_local_mut(&mut self, name: &LocalName) -> Option<&mut Local> {
        if matches!(name, LocalName::Anonymous) {
            return None; // anonymous temporaries should not be referenced
        }
        self.locals.iter_mut().find(|local| local.name == *name)
    }
    
    fn push_local(&mut self, mode: Access, name: LocalName) -> CompileResult<&Local> {
        let index = self.last_index().map_or(
            Ok(0),
            |index| index.checked_add(1)
                .ok_or("local variable limit reached")
        )?;
        
        let local = Local {
            mode, name, index, 
            captured: false,
        };
        
        self.locals.push(local);
        Ok(self.locals.last().unwrap())
    }
    
    fn insert_local(&mut self, mode: Access, name: LocalName) -> CompileResult<InsertLocal> {
        // see if this local already exists in the current scope
        if let Some(mut local) = self.find_local_mut(&name) {
            (*local).mode = mode; // redeclare with new mutability
            Ok(InsertLocal::HideExisting(local.index))
        } else {
            let local = self.push_local(mode, name)?;
            Ok(InsertLocal::CreateNew(local.index))
        }
    }
}


#[derive(Debug)]
struct NestedScopes {
    toplevel: Scope,
    nested: Vec<Scope>,
}

impl NestedScopes {
    fn new(symbol: Option<&DebugSymbol>, tag: ScopeTag, label: Option<Label>) -> Self {
        let toplevel = Scope {
            tag,
            depth: 0,
            prev_index: None,
            symbol: symbol.copied(),
            locals: Vec::new(),
            control_flow: ControlFlowTracker::new(label),
        };
        
        Self {
            toplevel,
            nested: Vec::new(),
        }
    }
    
    fn is_nested(&self) -> bool {
        !self.nested.is_empty()
    }
    
    fn current_scope(&self) -> &Scope {
        self.nested.last().unwrap_or(&self.toplevel)
    }
    
    fn current_scope_mut(&mut self) -> &mut Scope {
        self.nested.last_mut().unwrap_or(&mut self.toplevel)
    }
    
    fn push_scope(&mut self, symbol: Option<&DebugSymbol>, tag: ScopeTag, label: Option<Label>) {
        let current_scope = self.current_scope();
        
        let scope = Scope {
            tag,
            depth: current_scope.depth() + 1,
            prev_index: current_scope.last_index(),
            symbol: symbol.copied(),
            locals: Vec::new(),
            control_flow: ControlFlowTracker::new(label),
        };
        
        self.nested.push(scope);
    }
    
    fn pop_scope(&mut self) -> Scope {
        self.nested.pop().expect("pop toplevel scope")
    }
    
    /// Iterate in name resolution order
    fn iter_nro(&self) -> impl Iterator<Item=&Scope> {
        self.nested.iter().rev()
            .chain(std::iter::once(&self.toplevel))
    }
    
    fn iter_nro_mut(&mut self) -> impl Iterator<Item=&mut Scope> {
        self.nested.iter_mut().rev()
            .chain(std::iter::once(&mut self.toplevel))
    }
}


#[derive(Debug, Clone)]
pub(super) struct Upvalue {
    mode: Access,
    name: LocalName,
    index: UpvalueIndex,
    target: UpvalueTarget,
}

impl Upvalue {
    pub(super) fn mode(&self) -> Access { self.mode }
    pub(super) fn name(&self) -> LocalName { self.name }
    pub(super) fn index(&self) -> UpvalueIndex { self.index }
    pub(super) fn target(&self) -> UpvalueTarget { self.target }
}


#[derive(Debug)]
pub(super) struct CallFrame {
    scopes: NestedScopes,
    upvalues: Vec<Upvalue>,
}

impl CallFrame {
    fn new(symbol: Option<&DebugSymbol>) -> Self {
        Self {
            scopes: NestedScopes::new(symbol, ScopeTag::Function, None),
            upvalues: Vec::new(),
        }
    }
    
    pub(super) fn upvalues(&self) -> &[Upvalue] { self.upvalues.as_slice() }
    
    pub(super) fn iter_locals(&self) -> impl Iterator<Item=&Local> {
        self.scopes().iter_nro().flat_map(|scope| scope.locals().iter())
    }
    
    fn scopes(&self) -> &NestedScopes { &self.scopes }
    
    fn scopes_mut(&mut self) -> &mut NestedScopes { &mut self.scopes }
    
    fn find_upval(&self, name: &LocalName) -> Option<&Upvalue> {
        self.upvalues.iter().find(|upval| upval.name == *name)
    }
    
    fn create_upval_for_local(&mut self, local: &mut Local) -> CompileResult<&Upvalue> {
        let index = UpvalueIndex::try_from(self.upvalues.len())
            .map_err(|_| "upvalue limit reached")?;
        
        let upval = Upvalue {
            index,
            mode: local.mode,
            name: local.name,
            target: UpvalueTarget::Local(local.index),
        };
        self.upvalues.push(upval);
        
        local.captured = true;
        
        Ok(self.upvalues.last().unwrap())
    }
    
    fn create_upval_for_upval(&mut self, upval: &Upvalue) -> CompileResult<&Upvalue> {
        let index = UpvalueIndex::try_from(self.upvalues.len())
            .map_err(|_| "upvalue limit reached")?;
        
        let upval = Upvalue {
            index,
            mode: upval.mode,
            name: upval.name,
            target: UpvalueTarget::Upvalue(upval.index),
        };
        
        self.upvalues.push(upval);
        
        Ok(self.upvalues.last().unwrap())
    }
}


#[derive(Debug)]
pub(super) struct ScopeTracker {
    toplevel: NestedScopes,
    frames: Vec<CallFrame>,
}

impl ScopeTracker {
    pub(super) fn new() -> Self {
        Self {
            toplevel: NestedScopes::new(None, ScopeTag::Global, None),
            frames: Vec::new(),
        }
    }
    
    pub(super) fn is_global_scope(&self) -> bool {
        self.current_scope().tag() == ScopeTag::Global
    }
    
    pub(super) fn is_call_frame(&self) -> bool {
        !self.frames.is_empty()
    }
    
    pub(super) fn push_frame(&mut self, symbol: Option<&DebugSymbol>) {
        self.frames.push(CallFrame::new(symbol))
    }
    
    pub(super) fn pop_frame(&mut self) -> CallFrame {
        self.frames.pop().expect("pop empty frames")
    }
    
    fn local_scopes(&self) -> &NestedScopes {
        self.frames.last()
            .map_or(&self.toplevel, |frame| frame.scopes())
    }
    
    fn local_scopes_mut(&mut self) -> &mut NestedScopes {
        self.frames.last_mut()
            .map_or(&mut self.toplevel, |frame| frame.scopes_mut())
    }
    
    // scopes
    
    pub(super) fn current_scope(&self) -> &Scope {
        self.local_scopes().current_scope()
    }
    
    pub(super) fn current_scope_mut(&mut self) -> &mut Scope {
        self.local_scopes_mut().current_scope_mut()
    }
    
    pub(super) fn push_scope(&mut self, symbol: Option<&DebugSymbol>, tag: ScopeTag, label: Option<Label>) {
        self.local_scopes_mut().push_scope(symbol, tag, label);
    }
    
    pub(super) fn pop_scope(&mut self) -> Scope {
        let scope = self.local_scopes_mut().pop_scope();
        scope
    }
    
    // local variables
    
    pub(super) fn insert_local(&mut self, mode: Access, name: LocalName) -> CompileResult<InsertLocal> {
        self.current_scope_mut().insert_local(mode, name)
    }
    
    pub(super) fn resolve_local(&self, name: &LocalName) -> Option<&Local> {
        self.local_scopes()
            .iter_nro().find_map(|scope| scope.find_local(name))
    }
    
    // upvalues
    
    pub(super) fn resolve_or_create_upval(&mut self, name: &LocalName) -> CompileResult<Option<&Upvalue>> {
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
            let enclosing = enclosing_frame.map_or(&mut self.toplevel, |frame| frame.scopes_mut());
            if let Some(local) = enclosing.iter_nro_mut().find_map(|scope| scope.find_local_mut(name)) {
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
    fn get_frames_mut(frames: &mut [CallFrame], frame_idx: usize) -> (&mut CallFrame, Option<&mut CallFrame>) {
        let (frames, _) = frames.split_at_mut(frame_idx + 1);
        let (current_frame, frames) = frames.split_last_mut().unwrap();
        let enclosing_frame = frames.split_last_mut().map(|(last, _)| last);
        (current_frame, enclosing_frame)
    }
    
    // control flow
    
    // search for a scope that matches the given control flow and label
    pub(super) fn resolve_control_flow(&self, target: ControlFlowTarget) -> Option<&Scope> {
        self.local_scopes()
            .iter_nro()
            .find_map(|scope| {
                if scope.tag().accepts_control_flow(target) {
                    if target.label().is_none() || target.label() == scope.control_flow.label.as_ref() {
                        return Some(scope)
                    }
                }
                None
            })
    }
    
    pub(super) fn iter_scopes(&self) -> impl Iterator<Item=&Scope> {
        self.local_scopes().iter_nro()
    }
    
    pub(super) fn iter_scopes_mut(&mut self) -> impl Iterator<Item=&mut Scope> {
        self.local_scopes_mut().iter_nro_mut()
    }
}