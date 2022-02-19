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
    Var, Begin, 
    If, Then, Elif, Else,
    For, While, Do,
    Fun, Class,
    Self_, Super,
    Echo,
    End,
    
    // Literals
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(i32),
    FloatLiteral(f32),
    
    // Misc
    Comment,
    EOF,
}
