use core::fmt;
use crate::runtime::types::operator;
use crate::runtime::types::operator::{Arithmetic, Bitwise, Shift, Comparison, Logical};

// Unary Operators

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg, Pos, Inv, Not,
}

// Binary Operators

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // precedence level 1
    Mul, Div, Mod,
    
    // precedence level 2
    Add, Sub,
    
    // precedence level 3
    LShift, RShift,
    
    // precedence level 4
    BitAnd,
    
    // precedence level 5
    BitXor,
    
    // precedence level 6
    BitOr,
    
    // precedence level 7
    LT, GT, LE, GE,
    
    // precedence level 8
    EQ, NE,
    
    // precedence level 9
    And,
    
    // precedence level 10
    Or,
}

pub type Precedence = u8;
pub const PRECEDENCE_END: Precedence = 0; // tightest binding
pub const PRECEDENCE_START: Precedence = 10; // weakest binding

impl BinaryOp {
    
    pub const fn precedence_level(&self) -> Precedence {
        match self {
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 1,
            
            BinaryOp::Add | BinaryOp::Sub => 2,
            
            BinaryOp::LShift | BinaryOp::RShift => 3,
            
            BinaryOp::BitAnd => 4,
            BinaryOp::BitXor => 5,
            BinaryOp::BitOr => 6,
            
            BinaryOp::LT | BinaryOp::GT | BinaryOp::LE | BinaryOp::GE  => 7,
            BinaryOp::EQ | BinaryOp::NE => 8,
            
            BinaryOp::And => 9,
            BinaryOp::Or => 10,
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let symbol = match self {
            BinaryOp::Mul    => "*", 
            BinaryOp::Div    => "/", 
            BinaryOp::Mod    => "%",
            BinaryOp::Add    => "+",
            BinaryOp::Sub    => "-",
            BinaryOp::LShift => "<<", 
            BinaryOp::RShift => ">>",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitXor => "^",
            BinaryOp::BitOr  => "|",
            BinaryOp::LT     => "<",
            BinaryOp::GT     => ">",
            BinaryOp::LE     => "<=",
            BinaryOp::GE     => ">=",
            BinaryOp::EQ     => "==",
            BinaryOp::NE     => "!=",
            BinaryOp::And    => "and",
            BinaryOp::Or     => "or",
        };
        fmt.write_str(symbol)
    }
}

// Conversion to runtime operator types

impl From<UnaryOp> for operator::UnaryOp {
    fn from(op: UnaryOp) -> Self {
        match op {
            UnaryOp::Neg => Self::Neg,
            UnaryOp::Pos => Self::Pos,
            UnaryOp::Inv => Self::Inv,
            UnaryOp::Not => Self::Not,
        }
    }
}

impl From<BinaryOp> for operator::BinaryOp {
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