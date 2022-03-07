use crate::debug::symbol::DebugSymbol;
use crate::parser::expr::ExprVariant;


#[derive(Debug, Clone)]
pub enum StmtVariant {
    
    // WhileLoop
    // DoWhileLoop
    // ForLoop
    Expression(ExprVariant),
    
    // Continue
    // Break
    // Return
    
    Echo(ExprVariant),
}

impl StmtVariant {
}

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
