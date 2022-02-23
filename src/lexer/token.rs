use crate::language;

// Token Types

#[derive(Clone, Debug)]
pub enum Token {
    // Delimiters and Separators
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
    
    // Operator Symbols
    OpAdd, OpSub, OpMul, OpDiv, OpMod,
    OpAnd, OpOr, OpXor, OpLShift, OpRShift,
    
    OpAddAssign, OpSubAssign, OpMulAssign, OpDivAssign, OpModAssign,
    OpAndAssign, OpOrAssign, OpXorAssign, OpLShiftAssign, OpRShiftAssign,
    
    OpLT, OpLE, OpGT, OpGE, OpEQ, OpNE,
    OpAssign, OpAccess,
    
    // Keywords
    And, Or, Not,
    True, False, Nil,
    Var, Global, Del,
    If, Then, Elif, Else,
    Begin, For, While, Do,
    Fun, Class,
    Self_, Super,
    Echo,
    End,
    
    // Literals
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(language::IntType),
    FloatLiteral(language::FloatType),
    
    // Misc
    Comment,
    EOF,
}
