use crate::parser::primary::Primary;
use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::parser::structs::{ObjectConstructor};
use crate::parser::debug::{DebugSymbol, HasDebugSymbol};

#[derive(Debug, Clone)]
pub enum Expr {
    
    Primary(Box<Primary>),
    
    UnaryOp(UnaryOp, Box<Expr>),
    
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    
    Assignment(Box<AssignmentInfo>), // use a box to keep size of Expr down
    
    ObjectCtor(Box<ObjectConstructor>),
    
    TupleCtor(Vec<Expr>),
    
}

impl Expr {
    pub fn primary(primary: Primary) -> Self {
        Self::Primary(Box::new(primary))
    }
    
    pub fn unary_op(op: UnaryOp, expr: Expr) -> Self {
        Self::UnaryOp(op, Box::new(expr))
    }
    
    pub fn binary_op(op: BinaryOp, lhs: Expr, rhs: Expr) -> Self {
        Self::BinaryOp(op, Box::new(lhs), Box::new(rhs))
    }
    
    pub fn assignment(lhs: Primary, op: Option<BinaryOp>, rhs: Expr) -> Self {
        debug_assert!(lhs.is_lvalue());
        
        Self::Assignment(Box::new(AssignmentInfo { lhs, op, rhs }))
    }
    
    // pub fn object_ctor
}

#[derive(Debug, Clone)]
pub struct AssignmentInfo {
    lhs: Primary,
    op: Option<BinaryOp>, // e.g. for +=, -=, *=, ...
    rhs: Expr,
}


#[derive(Debug, Clone)]
pub struct ExprMeta<'a> {
    expr: Expr,
    debug: DebugSymbol<'a>,
}

impl<'a> ExprMeta<'a> {
    pub fn new(expr: Expr, debug: DebugSymbol<'a>) -> Self {
        ExprMeta { expr, debug }
    }
    
    pub fn expr(&self) -> &Expr { &self.expr }
}

impl HasDebugSymbol for ExprMeta<'_> {
    fn debug_symbol(&self) -> &DebugSymbol { &self.debug }
}