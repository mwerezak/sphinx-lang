use crate::parser::primary::Primary;
use crate::parser::operator::{BinaryOp, UnaryOp};


#[derive(Debug)]
pub enum Expr {
    Primary(Primary),
    UnaryOp(UnaryOp, Box<Expr>),
    BinOp(BinaryOp, Box<Expr>, Box<Expr>),
}

