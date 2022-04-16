use core::fmt;


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
