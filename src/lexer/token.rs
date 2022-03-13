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
    OpInv, OpAnd, OpOr, OpXor, OpLShift, OpRShift,
    
    OpAddAssign, OpSubAssign, OpMulAssign, OpDivAssign, OpModAssign,
    OpAndAssign, OpOrAssign, OpXorAssign, OpLShiftAssign, OpRShiftAssign,
    
    OpLT, OpLE, OpGT, OpGE, OpEQ, OpNE,
    OpAssign, OpAccess,
    
    // Keywords
    And, Or, Not,
    True, False, Nil,
    Let, Var, NonLocal, Del,
    If, Then, Elif, Else,
    Begin, For, In, While, Do,
    Continue, Break, Return,
    Fun, Class,
    // Self_, Super,
    Echo, Assert,
    End,
    
    // Literals
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(language::IntType),
    FloatLiteral(language::FloatType),
    
    // Misc
    Label(String),
    Comment,
    EOF,
}


// Token Output

// Max source file length ~4 billion characters (assuming mostly single byte UTF8 that's a ~4GB file)
// Max token length 65535 characters
pub type TokenIndex = u32;
pub type TokenLength = u16;

// include only mere character indexes in the output
// if a lexeme needs to be rendered (e.g. for error messages), 
// the relevant string can be extracted from the source at that point
#[derive(Clone, Debug)]
pub struct Span {
    pub index: TokenIndex,  // index of start of token in file
    pub length: TokenLength,
}

impl Span {
    pub fn start_index(&self) -> TokenIndex { self.index }
    pub fn end_index(&self) -> TokenIndex { self.index + TokenIndex::from(self.length) }
}

#[derive(Clone, Debug)]
pub struct TokenMeta {
    pub token: Token,
    pub span: Span,
    pub newline: bool,  // true if this is the first token after the start of a new line
}