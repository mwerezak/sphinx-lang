// Token Types

pub enum Token {
    Delim(Delimiter),
    OpSym(OpSymbol),
    Keyword(Keyword),
    
    StringLiteral(String),
    IntegerLiteral(u32),
    FloatLiteral(f32),
    
    EOF,
}

pub enum Delimiter {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenSquare,
    CloseSquare,
    Comma,
    Colon,
    Semicolon,
}

pub enum OpSymbol {
    Add, Sub, Mul, Div, Mod,
    BitAnd, BitOr, BitXor, LShift, RShift,
    LT, LE, GT, GE, EQ, NE,
    And, Or, Not,
    Assign, Access,
}

pub enum Keyword {
    Var, Begin, 
    If, Then, Elif, Else,
    For, While, Do,
    Fun, Class,
    Self_, Super, True, False, Nil,
    Echo,
    End,
}