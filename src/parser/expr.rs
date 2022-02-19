use crate::parser::primary::Primary;
use crate::parser::binop::BinaryOp;


#[derive(Debug)]
pub enum Expr {
    Primary(Primary),
    BinOp(BinaryOp, Box<Expr>, Box<Expr>),
}

