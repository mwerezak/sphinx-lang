use crate::lexer::LexerBuilder;
use crate::lexer::Token;
use crate::lexer::{SingleCharRule, ExactRule};


pub fn create_default_lexer_rules() -> LexerBuilder {
    let mut builder = LexerBuilder::new();
    
    builder
    .add_rule(SingleCharRule::new(Token::OpenParen,   '('))
    .add_rule(SingleCharRule::new(Token::CloseParen,  ')'))
    .add_rule(SingleCharRule::new(Token::OpenBrace,   '{'))
    .add_rule(SingleCharRule::new(Token::CloseBrace,  '}'))
    .add_rule(SingleCharRule::new(Token::OpenSquare,  '['))
    .add_rule(SingleCharRule::new(Token::CloseSquare, ']'))
    .add_rule(SingleCharRule::new(Token::Comma,       ','))
    .add_rule(SingleCharRule::new(Token::Colon,       ':'))
    .add_rule(SingleCharRule::new(Token::Semicolon,   ';'))

    .add_rule(SingleCharRule::new(Token::OpAssign,    '='))
    .add_rule(SingleCharRule::new(Token::OpAccess,    '.'))
    
    .add_rule(SingleCharRule::new(Token::OpAdd,       '+'))
    .add_rule(SingleCharRule::new(Token::OpSub,       '-'))
    .add_rule(SingleCharRule::new(Token::OpMul,       '*'))
    .add_rule(SingleCharRule::new(Token::OpDiv,       '/'))
    .add_rule(SingleCharRule::new(Token::OpMod,       '%'))
    .add_rule(SingleCharRule::new(Token::OpAnd,       '&'))
    .add_rule(SingleCharRule::new(Token::OpOr,        '|'))
    .add_rule(SingleCharRule::new(Token::OpXor,       '^'))
    
    .add_rule(ExactRule::new(Token::OpLShift,         "<<"))
    .add_rule(ExactRule::new(Token::OpRShift,         ">>"))
    
    .add_rule(ExactRule::new(Token::OpAddAssign,      "+="))
    .add_rule(ExactRule::new(Token::OpSubAssign,      "-="))
    .add_rule(ExactRule::new(Token::OpMulAssign,      "*="))
    .add_rule(ExactRule::new(Token::OpDivAssign,      "/="))
    .add_rule(ExactRule::new(Token::OpModAssign,      "%="))
    .add_rule(ExactRule::new(Token::OpAndAssign,      "&="))
    .add_rule(ExactRule::new(Token::OpOrAssign,       "|="))
    .add_rule(ExactRule::new(Token::OpLShiftAssign,   "<<="))
    .add_rule(ExactRule::new(Token::OpRShiftAssign,   ">>="))
    
    .add_rule(SingleCharRule::new(Token::OpLT,        '<'))
    .add_rule(SingleCharRule::new(Token::OpGT,        '>'))
    
    .add_rule(ExactRule::new(Token::OpLE,             "<="))
    .add_rule(ExactRule::new(Token::OpGE,             ">="))
    .add_rule(ExactRule::new(Token::OpEQ,             "=="))
    .add_rule(ExactRule::new(Token::OpNE,             "!="))
    
    .add_rule(ExactRule::new(Token::And,              "and"))
    .add_rule(ExactRule::new(Token::Or,               "or"))
    .add_rule(ExactRule::new(Token::Not,              "not"))
    .add_rule(ExactRule::new(Token::True,             "true"))
    .add_rule(ExactRule::new(Token::False,            "false"))
    .add_rule(ExactRule::new(Token::Nil,              "nil"))
    .add_rule(ExactRule::new(Token::Var,              "var"))
    .add_rule(ExactRule::new(Token::Begin,            "begin"))
    .add_rule(ExactRule::new(Token::If,               "if"))
    .add_rule(ExactRule::new(Token::Then,             "then"))
    .add_rule(ExactRule::new(Token::Elif,             "elif"))
    .add_rule(ExactRule::new(Token::Else,             "else"))
    .add_rule(ExactRule::new(Token::For,              "for"))
    .add_rule(ExactRule::new(Token::While,            "while"))
    .add_rule(ExactRule::new(Token::Do,               "do"))
    .add_rule(ExactRule::new(Token::Fun,              "fun"))
    .add_rule(ExactRule::new(Token::Class,            "class"))
    .add_rule(ExactRule::new(Token::Self_,            "self"))
    .add_rule(ExactRule::new(Token::Super,            "super"))
    .add_rule(ExactRule::new(Token::Echo,             "echo"))
    .add_rule(ExactRule::new(Token::End,              "end"))
    
    ;
    
    return builder;
}