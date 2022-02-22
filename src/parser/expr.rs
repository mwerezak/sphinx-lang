use crate::parser::primary::Primary;
use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::parser::structs::{ObjectConstructor};
use crate::parser::debug::{DebugMeta, DebugInfo};


#[derive(Debug, Clone)]
pub enum Expr {
    
    Primary(Primary),
    
    UnaryOp(UnaryOp, Box<Expr>),
    
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    
    Assignment {
        decl: bool, // if this is a var-declaration
        lhs: Primary,
        op: Option<BinaryOp>, // e.g. for +=, -=, *=, ...
        expr: Box<Expr>,
    },
    
    ObjectCtor(ObjectConstructor),
    
    TupleCtor(Vec<Expr>),
}

impl Expr {
    pub fn unary_op(op: UnaryOp, expr: Expr) -> Self {
        Self::UnaryOp(op, Box::new(expr))
    }
    
    pub fn binary_op(op: BinaryOp, lhs: Expr, rhs: Expr) -> Self {
        Self::BinaryOp(op, Box::new(lhs), Box::new(rhs))
    }
    
    pub fn assignment(lhs: Primary, op: Option<BinaryOp>, rhs: Expr, decl: bool) -> Self {
        debug_assert!(lhs.is_lvalue());
        Self::Assignment {
            lhs, op, decl,
            expr: Box::new(rhs),
        }
    }
}


// impl DebugInfo for Expr {
//     fn dbg_info(&self) -> &DebugMeta { }
//     fn dbg_info_mut(&self) -> &mut DebugMeta { }
//     fn set_dbg_info(&mut self, info: DebugMeta) { }
// }