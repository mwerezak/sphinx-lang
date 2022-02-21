use crate::parser::primary::Primary;
use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::parser::debug::{DebugMeta, DebugInfo};


#[derive(Debug, Clone)]
pub enum Expr {
    
    Primary(Primary),
    
    UnaryOp(UnaryOp, Box<Expr>),
    
    BinOp(BinaryOp, Box<Expr>, Box<Expr>),
    
    Assignment {
        decl: bool, // if this is a var-declaration
        lvalue: Primary,
        op: Option<BinaryOp>, // e.g. for +=, -=, *=, ...
        expr: Box<Expr>,
    },
}

impl Expr {
    pub fn assignment(lvalue: Primary, op: Option<BinaryOp>, expr: Expr, decl: bool) -> Self {
        debug_assert!(lvalue.is_lvalue());
        Self::Assignment {
            lvalue, op, decl,
            expr: Box::new(expr),
        }
    }
}


// impl DebugInfo for Expr {
//     fn dbg_info(&self) -> &DebugMeta { }
//     fn dbg_info_mut(&self) -> &mut DebugMeta { }
//     fn set_dbg_info(&mut self, info: DebugMeta) { }
// }