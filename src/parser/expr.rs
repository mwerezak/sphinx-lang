use crate::debug::symbol::DebugSymbol;
use crate::runtime::types::operator::{BinaryOp, UnaryOp};
use crate::parser::primary::Primary;
use crate::parser::assign::{Assignment, Declaration};
use crate::parser::structs::{ObjectConstructor};
use crate::parser::stmt::{Stmt, StmtVariant, Label};


#[derive(Debug, Clone)]
pub enum ExprVariant {
    
    Primary(Box<Primary>),
    
    UnaryOp(UnaryOp, Box<ExprVariant>),
    
    BinaryOp(BinaryOp, Box<(ExprVariant, ExprVariant)>),
    
    Assignment(Box<Assignment>), // use a box to keep size of Expr down
    
    Declaration(Box<Declaration>),

    Tuple(Vec<Expr>),
    
    ObjectCtor(Box<ObjectConstructor>),
    
    // IfExpr
    
    Block(Vec<Stmt>, Option<Label>), // TODO label
    
    // FunctionDef
    // ClassDef
}


// Use Expr instead of ExprVariant when we want to capture a debug symbol
// This should be for: 
//      top-level expressions like if/while conditions and statement expressions
//      innner expressions in tuples and [] indexing,
//      function arguments,
//      object constructor initializers,


#[derive(Debug, Clone)]
pub struct Expr {
    variant: ExprVariant,
    symbol: DebugSymbol,
}

impl Expr {
    pub fn new(variant: ExprVariant, symbol: DebugSymbol) -> Self {
        Expr { variant, symbol }
    }
    
    pub fn variant(&self) -> &ExprVariant { &self.variant }
    pub fn take_variant(self) -> ExprVariant { self.variant }
    
    pub fn replace_variant(&mut self, variant: ExprVariant) -> ExprVariant { 
        std::mem::replace(&mut self.variant, variant)
    }
    
    pub fn debug_symbol(&self) -> &DebugSymbol { &self.symbol }
}

impl From<Expr> for (ExprVariant, DebugSymbol) {
    fn from(expr: Expr) -> Self { (expr.variant, expr.symbol) }
}

// create an expression-statement

impl From<Expr> for Stmt {
    fn from(expr: Expr) -> Self {
        let variant = StmtVariant::Expression(expr.variant);
        Stmt::new(variant, expr.symbol)
    }
}