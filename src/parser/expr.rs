use crate::debug::symbol::DebugSymbol;
use crate::parser::primary::Primary;
use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::parser::structs::{ObjectConstructor};

#[derive(Debug, Clone)]
pub enum ExprVariant {
    
    Primary(Box<Primary>),
    
    UnaryOp(UnaryOp, Box<ExprVariant>),
    
    BinaryOp(BinaryOp, Box<ExprVariant>, Box<ExprVariant>),
    
    Assignment(Box<AssignmentInfo>), // use a box to keep size of Expr down
    
    Tuple(Vec<Expr>),
    
    ObjectCtor(Box<ObjectConstructor>),
    
}

#[derive(Debug, Clone)]
pub struct AssignmentInfo {
    pub lhs: Primary,
    pub op: Option<BinaryOp>, // e.g. for +=, -=, *=, ...
    pub rhs: ExprVariant,
}

impl ExprVariant {
    pub fn primary(primary: Primary) -> Self {
        Self::Primary(Box::new(primary))
    }
    
    pub fn unary_op(op: UnaryOp, expr: ExprVariant) -> Self {
        Self::UnaryOp(op, Box::new(expr))
    }
    
    pub fn binary_op(op: BinaryOp, lhs: ExprVariant, rhs: ExprVariant) -> Self {
        Self::BinaryOp(op, Box::new(lhs), Box::new(rhs))
    }
    
    pub fn assignment(lhs: Primary, op: Option<BinaryOp>, rhs: ExprVariant) -> Self {
        debug_assert!(lhs.is_lvalue());
        Self::Assignment(Box::new(AssignmentInfo { lhs, op, rhs }))
    }
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
    
    pub fn debug_symbol(&self) -> &DebugSymbol { &self.symbol }
}

