#![cfg(test)]

use super::literals::{Identifier, IntLiteral};
use super::binop::{BinaryOp, BinaryOpExpr};

#[test]
fn binop_test_pretty_print() {
    let foo = Identifier::new("foo");
    let bar = Identifier::new("bar");
    let expr1 = BinaryOpExpr::new(BinaryOp::Add, foo, bar);
    assert!(format!("{}", expr1) == "(+ foo bar)");
    
    let baz = Identifier::new("baz");
    let expr2 = BinaryOpExpr::new(BinaryOp::Div, expr1, baz);
    assert!(format!("{}", expr2) == "(/ (+ foo bar) baz)");
    
    let expr3 = BinaryOpExpr::new(BinaryOp::Mul, IntLiteral::new(2), expr2);
    assert!(format!("{}", expr3) == "(* 2 (/ (+ foo bar) baz))");
}