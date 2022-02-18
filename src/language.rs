use crate::lexer::{LexerBuilder, Token};
use crate::lexer::rules::*;

pub static COMMENT_CHAR: char = '#';

pub static NESTED_COMMENT_START: &'static str = "#{";
pub static NESTED_COMMENT_END:   &'static str = "}#";

pub fn create_default_lexer_rules() -> LexerBuilder {
    LexerBuilder::new()
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
    
    .add_rule(MultiCharRule::new(Token::OpLShift,         "<<"))
    .add_rule(MultiCharRule::new(Token::OpRShift,         ">>"))
    
    .add_rule(MultiCharRule::new(Token::OpAddAssign,      "+="))
    .add_rule(MultiCharRule::new(Token::OpSubAssign,      "-="))
    .add_rule(MultiCharRule::new(Token::OpMulAssign,      "*="))
    .add_rule(MultiCharRule::new(Token::OpDivAssign,      "/="))
    .add_rule(MultiCharRule::new(Token::OpModAssign,      "%="))
    .add_rule(MultiCharRule::new(Token::OpAndAssign,      "&="))
    .add_rule(MultiCharRule::new(Token::OpOrAssign,       "|="))
    .add_rule(MultiCharRule::new(Token::OpLShiftAssign,   "<<="))
    .add_rule(MultiCharRule::new(Token::OpRShiftAssign,   ">>="))
    
    .add_rule(SingleCharRule::new(Token::OpLT,        '<'))
    .add_rule(SingleCharRule::new(Token::OpGT,        '>'))
    
    .add_rule(MultiCharRule::new(Token::OpLE,             "<="))
    .add_rule(MultiCharRule::new(Token::OpGE,             ">="))
    .add_rule(MultiCharRule::new(Token::OpEQ,             "=="))
    .add_rule(MultiCharRule::new(Token::OpNE,             "!="))
    
    .add_rule(MultiCharRule::new(Token::And,              "and"))
    .add_rule(MultiCharRule::new(Token::Or,               "or"))
    .add_rule(MultiCharRule::new(Token::Not,              "not"))
    .add_rule(MultiCharRule::new(Token::True,             "true"))
    .add_rule(MultiCharRule::new(Token::False,            "false"))
    .add_rule(MultiCharRule::new(Token::Nil,              "nil"))
    .add_rule(MultiCharRule::new(Token::Var,              "var"))
    .add_rule(MultiCharRule::new(Token::Begin,            "begin"))
    .add_rule(MultiCharRule::new(Token::If,               "if"))
    .add_rule(MultiCharRule::new(Token::Then,             "then"))
    .add_rule(MultiCharRule::new(Token::Elif,             "elif"))
    .add_rule(MultiCharRule::new(Token::Else,             "else"))
    .add_rule(MultiCharRule::new(Token::For,              "for"))
    .add_rule(MultiCharRule::new(Token::While,            "while"))
    .add_rule(MultiCharRule::new(Token::Do,               "do"))
    .add_rule(MultiCharRule::new(Token::Fun,              "fun"))
    .add_rule(MultiCharRule::new(Token::Class,            "class"))
    .add_rule(MultiCharRule::new(Token::Self_,            "self"))
    .add_rule(MultiCharRule::new(Token::Super,            "super"))
    .add_rule(MultiCharRule::new(Token::Echo,             "echo"))
    .add_rule(MultiCharRule::new(Token::End,              "end"))
}