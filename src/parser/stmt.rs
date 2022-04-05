use crate::language::InternSymbol;
use crate::debug::DebugSymbol;
use crate::parser::expr::Expr;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Label(InternSymbol);

impl Label {
    pub fn new(name: InternSymbol) -> Self { Label(name) }
    pub fn name(&self) -> &InternSymbol { &self.0 }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    
    Expression(Expr),
    
    Loop {
        label: Option<Label>, 
        body: StmtList, 
    },
    
    WhileLoop {
        label: Option<Label>, 
        condition: Expr,
        body: StmtList,
    },
    
    ForLoop { },
    
    Assert(Expr),
}


// Statement blocks 
// (called "statement lists" in Sphinx so as not to be confused with "block expressions")
#[derive(Debug, Clone)]
pub struct StmtList {
    suite: Box<[StmtMeta]>,
    control: Option<ControlFlow>,
}

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Continue(Option<Label>, Option<DebugSymbol>),
    Break(Option<Label>, Option<Box<Expr>>, Option<DebugSymbol>),
    Return(Option<Box<Expr>>, Option<DebugSymbol>),
}

impl StmtList {
    pub fn new(suite: Vec<StmtMeta>, control: Option<ControlFlow>) -> Self {
        Self {
            suite: suite.into_boxed_slice(),
            control,
        }
    }
    
    pub fn iter(&self) -> impl Iterator<Item=&StmtMeta> {
        self.suite.iter()
    }
    
    pub fn end_control(&self) -> Option<&ControlFlow> { self.control.as_ref() }
    
    pub fn take(self) -> (Vec<StmtMeta>, Option<ControlFlow>) {
        (self.suite.into_vec(), self.control)
    }
}


// Stmt + DebugSymbol
#[derive(Debug, Clone)]
pub struct StmtMeta {
    variant: Stmt,
    symbol: DebugSymbol,
}

impl StmtMeta {
    pub fn new(variant: Stmt, symbol: DebugSymbol) -> Self {
        StmtMeta { variant, symbol }
    }
    
    pub fn variant(&self) -> &Stmt { &self.variant }
    pub fn take_variant(self) -> Stmt { self.variant }
    
    pub fn debug_symbol(&self) -> &DebugSymbol { &self.symbol }
    pub fn take_symbol(self) -> DebugSymbol { self.symbol }
    
    pub fn take(self) -> (Stmt, DebugSymbol) { (self.variant, self.symbol) }
}
