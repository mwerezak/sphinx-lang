use crate::runtime::strings::StringSymbol;
use crate::debug::DebugSymbol;
use crate::parser::expr::Expr;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Label(StringSymbol);

impl Label {
    pub fn new(name: StringSymbol) -> Self { Label(name) }
    pub fn name(&self) -> &StringSymbol { &self.0 }
}


#[derive(Debug, Clone)]
pub enum Stmt {
    
    Expression(Expr),
    
    // WhileLoop
    // DoWhileLoop
    // ForLoop
    
    Continue(Option<Label>),
    Break(Option<Label>, Option<Expr>),
    Return(Option<Expr>),
    
    Echo(Expr),
    
}

impl Stmt { }

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
