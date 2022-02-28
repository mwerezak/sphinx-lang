use crate::parser::operator::{UnaryOp, BinaryOp};


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Unary {
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
pub enum Operator {
    Unary(Unary),
    Arithmetic(Arithmetic),
    Bitwise(Bitwise),
    Shift(Shift),
    Comparison(Comparison),
    Logical(Logical),
}

impl From<Unary> for Operator {
    fn from(op: Unary) -> Self { Self::Unary(op) }
}

impl From<Arithmetic> for Operator {
    fn from(op: Arithmetic) -> Self { Self::Arithmetic(op) }
}

impl From<Bitwise> for Operator {
    fn from(op: Bitwise) -> Self { Self::Bitwise(op) }
}

impl From<Shift> for Operator {
    fn from(op: Shift) -> Self { Self::Shift(op) }
}

impl From<Comparison> for Operator {
    fn from(op: Comparison) -> Self { Self::Comparison(op) }
}

impl From<Logical> for Operator {
    fn from(op: Logical) -> Self { Self::Logical(op) }
}

// Conversion from parser op types

impl From<UnaryOp> for Unary {
    fn from(op: UnaryOp) -> Self {
        match op {
            UnaryOp::Neg => Self::Neg,
            UnaryOp::Pos => Self::Pos,
            UnaryOp::Inv => Self::Inv,
            UnaryOp::Not => Self::Not,
        }
    }
}

impl From<BinaryOp> for Operator {
    fn from(op: BinaryOp) -> Self {
        match op {
            BinaryOp::Mul    => Self::Arithmetic(Arithmetic::Mul),
            BinaryOp::Div    => Self::Arithmetic(Arithmetic::Div),
            BinaryOp::Mod    => Self::Arithmetic(Arithmetic::Mod),
            BinaryOp::Add    => Self::Arithmetic(Arithmetic::Add),
            BinaryOp::Sub    => Self::Arithmetic(Arithmetic::Sub),
            BinaryOp::LShift => Self::Shift(Shift::Left),
            BinaryOp::RShift => Self::Shift(Shift::Right),
            BinaryOp::BitAnd => Self::Bitwise(Bitwise::And),
            BinaryOp::BitXor => Self::Bitwise(Bitwise::Xor),
            BinaryOp::BitOr  => Self::Bitwise(Bitwise::Or),
            BinaryOp::LT     => Self::Comparison(Comparison::LT),
            BinaryOp::GT     => Self::Comparison(Comparison::GT),
            BinaryOp::LE     => Self::Comparison(Comparison::LE),
            BinaryOp::GE     => Self::Comparison(Comparison::GE),
            BinaryOp::EQ     => Self::Comparison(Comparison::EQ),
            BinaryOp::NE     => Self::Comparison(Comparison::NE),
            BinaryOp::And    => Self::Logical(Logical::And),
            BinaryOp::Or     => Self::Logical(Logical::Or),
        }
    }
}