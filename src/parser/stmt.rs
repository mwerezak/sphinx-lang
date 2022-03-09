use crate::runtime::strings::InternSymbol;
use crate::debug::symbol::DebugSymbol;
use crate::parser::expr::ExprVariant;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Label(InternSymbol);

impl Label {
    pub fn new(name: InternSymbol) -> Self { Label(name) }
    pub fn name(&self) -> &InternSymbol { &self.0 }
}


#[derive(Debug, Clone)]
pub enum StmtVariant {
    
    Expression(ExprVariant),
    
    // WhileLoop
    // DoWhileLoop
    // ForLoop
    
    Continue(Option<Label>),
    Break(Option<Label>, Option<ExprVariant>),
    Return(Option<ExprVariant>),
    
    Echo(ExprVariant),
    
}

impl StmtVariant { }

#[derive(Debug, Clone)]
pub struct Stmt {
    variant: StmtVariant,
    symbol: DebugSymbol,
}

impl Stmt {
    pub fn new(variant: StmtVariant, symbol: DebugSymbol) -> Self {
        Stmt { variant, symbol }
    }
    
    pub fn variant(&self) -> &StmtVariant { &self.variant }
    pub fn take_variant(self) -> StmtVariant { self.variant }
    
    pub fn debug_symbol(&self) -> &DebugSymbol { &self.symbol }
}
