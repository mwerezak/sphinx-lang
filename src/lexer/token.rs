use crate::language::{IntType, FloatType};
use crate::debug::DebugSymbol;

// Token Types

#[derive(Clone, Debug)]
pub enum Token {
    // Delimiters, Separators, punctuation
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenSquare,
    CloseSquare,
    Comma,
    Colon,
    Semicolon,
    Ellipsis,
    Decorator,
    
    // Operator Symbols
    OpAdd, OpSub, OpMul, OpDiv, OpMod, OpExp,
    OpInv, OpAnd, OpOr, OpXor, OpLShift, OpRShift,
    
    OpAddAssign, OpSubAssign, OpMulAssign, OpDivAssign, OpModAssign,
    OpAndAssign, OpOrAssign, OpXorAssign, OpLShiftAssign, OpRShiftAssign,
    
    OpLT, OpLE, OpGT, OpGE, OpEQ, OpNE,
    OpAssign, OpAccess,
    
    // Keywords
    And, Or, Not,
    True, False, Nil,
    Let, Var, Local, NonLocal, Del,
    If, Then, Elif, Else,
    Begin, Loop, While, For, In, Do,
    Continue, Break, Return,
    Fun, Class,
    // Self_, Super,
    Echo, Assert,
    End,
    
    // Literals
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(IntType),
    FloatLiteral(FloatType),
    
    // Misc
    Label(String),
    Comment,
    EOF,
}


/// Token Output
#[derive(Clone, Debug)]
pub struct TokenMeta {
    pub token: Token,
    pub symbol: DebugSymbol,
    pub newline: bool,  // true if this is the first token after the start of a new line
}