use crate::parser::operator::{UnaryOp as ParserUnary, BinaryOp as ParserBinary};


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Unary(UnaryOp),
    Binary(BinaryOp),
}


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Pos, Neg, Inv, Not,
}


// Binary Operators

// Coercion rules
// arithmetic operations - short-circuit for int/float, coerce to float if either operand is float
// bitwise operations    - short-circuit for bool/int, coerce to int if either operand is int
// shift operations      - short-circuit if LHS is bool/int and RHS is int, result is always int
// inequality comparison - short-circuit for int/flot, result is always bool
// equality              - handled specially, result is always bool

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Arithmetic {
    Mul, Div, Mod,
    Add, Sub,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Bitwise {
    And, Xor, Or,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Shift {
    Left, Right,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Comparison {
    LT, GT, LE, GE, EQ, NE,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Logical {
    And, Or,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Arithmetic(Arithmetic),
    Bitwise(Bitwise),
    Shift(Shift),
    Comparison(Comparison),
    Logical(Logical),
}

impl From<Arithmetic> for BinaryOp {
    fn from(op: Arithmetic) -> Self { Self::Arithmetic(op) }
}

impl From<Bitwise> for BinaryOp {
    fn from(op: Bitwise) -> Self { Self::Bitwise(op) }
}

impl From<Shift> for BinaryOp {
    fn from(op: Shift) -> Self { Self::Shift(op) }
}

impl From<Comparison> for BinaryOp {
    fn from(op: Comparison) -> Self { Self::Comparison(op) }
}

impl From<Logical> for BinaryOp {
    fn from(op: Logical) -> Self { Self::Logical(op) }
}

// Conversion from parser op types

impl From<ParserUnary> for UnaryOp {
    fn from(op: ParserUnary) -> Self {
        match op {
            ParserUnary::Neg => Self::Neg,
            ParserUnary::Pos => Self::Pos,
            ParserUnary::Inv => Self::Inv,
            ParserUnary::Not => Self::Not,
        }
    }
}

impl From<ParserBinary> for BinaryOp {
    fn from(op: ParserBinary) -> Self {
        match op {
            ParserBinary::Mul    => Self::Arithmetic(Arithmetic::Mul),
            ParserBinary::Div    => Self::Arithmetic(Arithmetic::Div),
            ParserBinary::Mod    => Self::Arithmetic(Arithmetic::Mod),
            ParserBinary::Add    => Self::Arithmetic(Arithmetic::Add),
            ParserBinary::Sub    => Self::Arithmetic(Arithmetic::Sub),
            ParserBinary::LShift => Self::Shift(Shift::Left),
            ParserBinary::RShift => Self::Shift(Shift::Right),
            ParserBinary::BitAnd => Self::Bitwise(Bitwise::And),
            ParserBinary::BitXor => Self::Bitwise(Bitwise::Xor),
            ParserBinary::BitOr  => Self::Bitwise(Bitwise::Or),
            ParserBinary::LT     => Self::Comparison(Comparison::LT),
            ParserBinary::GT     => Self::Comparison(Comparison::GT),
            ParserBinary::LE     => Self::Comparison(Comparison::LE),
            ParserBinary::GE     => Self::Comparison(Comparison::GE),
            ParserBinary::EQ     => Self::Comparison(Comparison::EQ),
            ParserBinary::NE     => Self::Comparison(Comparison::NE),
            ParserBinary::And    => Self::Logical(Logical::And),
            ParserBinary::Or     => Self::Logical(Logical::Or),
        }
    }
}