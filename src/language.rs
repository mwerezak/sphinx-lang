use crate::lexer::{LexerBuilder, Token};
use crate::lexer::rules::{SingleCharRule, MultiCharRule};
use crate::lexer::rules::keywords::KeywordRule;
use crate::lexer::rules::literals::*;
use crate::lexer::rules::literals::string::*;


pub type IntType = i64;    // internal representation for integers
pub type FloatType = f64;  // internal representation for floats


pub static COMMENT_CHAR: char = '#';

pub static NESTED_COMMENT_START: &str = "#{";
pub static NESTED_COMMENT_END:   &str = "}#";

// string literal escape sequences - lazy initialized
lazy_static! {
    pub static ref ESCAPE_SEQUENCES: Vec<Box<dyn EscapeSequence>> = { 
        let mut escapes = Vec::<Box<dyn EscapeSequence>>::new();
        
        escapes.push(Box::new(CharMapEscape::new('0', "\x00")));
        escapes.push(Box::new(CharMapEscape::new('\\', "\\")));
        escapes.push(Box::new(CharMapEscape::new('\'', "\'")));
        escapes.push(Box::new(CharMapEscape::new('\"', "\"")));
        
        escapes.push(Box::new(CharMapEscape::new('t', "\t")));
        escapes.push(Box::new(CharMapEscape::new('n', "\n")));
        escapes.push(Box::new(CharMapEscape::new('r', "\r")));
        escapes.push(Box::new(HexByteEscape::new()));
        
        escapes
    };
}

pub fn create_default_lexer_rules() -> LexerBuilder {
    LexerBuilder::new()
    
    // Punctuation
    .add_rule(SingleCharRule::new(Token::OpenParen,       '('))
    .add_rule(SingleCharRule::new(Token::CloseParen,      ')'))
    .add_rule(SingleCharRule::new(Token::OpenBrace,       '{'))
    .add_rule(SingleCharRule::new(Token::CloseBrace,      '}'))
    .add_rule(SingleCharRule::new(Token::OpenSquare,      '['))
    .add_rule(SingleCharRule::new(Token::CloseSquare,     ']'))
    .add_rule(SingleCharRule::new(Token::Comma,           ','))
    .add_rule(SingleCharRule::new(Token::Colon,           ':'))
    .add_rule(SingleCharRule::new(Token::Semicolon,       ';'))
    .add_rule(SingleCharRule::new(Token::Decorator,       '@'))
    
    .add_rule(MultiCharRule::new(Token::Ellipsis,         "..."))
    
    // Assignment and access operators
    .add_rule(SingleCharRule::new(Token::OpAssign,        '='))
    .add_rule(SingleCharRule::new(Token::OpAccess,        '.'))
    
    // Arithmetic and comparison operators
    .add_rule(SingleCharRule::new(Token::OpAdd,           '+'))
    .add_rule(SingleCharRule::new(Token::OpSub,           '-'))
    .add_rule(SingleCharRule::new(Token::OpMul,           '*'))
    .add_rule(SingleCharRule::new(Token::OpDiv,           '/'))
    .add_rule(SingleCharRule::new(Token::OpMod,           '%'))
    .add_rule(SingleCharRule::new(Token::OpExp,           '^'))
    .add_rule(SingleCharRule::new(Token::OpInv,           '~'))
    .add_rule(SingleCharRule::new(Token::OpAnd,           '&'))
    .add_rule(SingleCharRule::new(Token::OpOr,            '|'))
    .add_rule(SingleCharRule::new(Token::OpXor,           '~'))
    
    .add_rule(SingleCharRule::new(Token::OpLT,            '<'))
    .add_rule(SingleCharRule::new(Token::OpGT,            '>'))
    
    .add_rule(MultiCharRule::new(Token::OpLE,             "<="))
    .add_rule(MultiCharRule::new(Token::OpGE,             ">="))
    .add_rule(MultiCharRule::new(Token::OpEQ,             "=="))
    .add_rule(MultiCharRule::new(Token::OpNE,             "!="))
    
    .add_rule(MultiCharRule::new(Token::OpAddAssign,      "+="))
    .add_rule(MultiCharRule::new(Token::OpSubAssign,      "-="))
    .add_rule(MultiCharRule::new(Token::OpMulAssign,      "*="))
    .add_rule(MultiCharRule::new(Token::OpDivAssign,      "/="))
    .add_rule(MultiCharRule::new(Token::OpModAssign,      "%="))
    .add_rule(MultiCharRule::new(Token::OpAndAssign,      "&="))
    .add_rule(MultiCharRule::new(Token::OpOrAssign,       "|="))
    .add_rule(MultiCharRule::new(Token::OpXorAssign,       "^="))
    .add_rule(MultiCharRule::new(Token::OpLShiftAssign,   "<<="))
    .add_rule(MultiCharRule::new(Token::OpRShiftAssign,   ">>="))
    
    .add_rule(MultiCharRule::new(Token::OpLShift,         "<<"))
    .add_rule(MultiCharRule::new(Token::OpRShift,         ">>"))
    
    // Keywords
    .add_rule(KeywordRule::new(Token::And,                "and"))
    .add_rule(KeywordRule::new(Token::Or,                 "or"))
    .add_rule(KeywordRule::new(Token::Not,                "not"))
    .add_rule(KeywordRule::new(Token::True,               "true"))
    .add_rule(KeywordRule::new(Token::False,              "false"))
    .add_rule(KeywordRule::new(Token::Nil,                "nil"))
    .add_rule(KeywordRule::new(Token::Let,                "let"))
    .add_rule(KeywordRule::new(Token::Var,                "var"))
    .add_rule(KeywordRule::new(Token::NonLocal,           "nonlocal"))
    .add_rule(KeywordRule::new(Token::Del,                "del"))
    .add_rule(KeywordRule::new(Token::Begin,              "begin"))
    .add_rule(KeywordRule::new(Token::If,                 "if"))
    .add_rule(KeywordRule::new(Token::Then,               "then"))
    .add_rule(KeywordRule::new(Token::Elif,               "elif"))
    .add_rule(KeywordRule::new(Token::Else,               "else"))
    .add_rule(KeywordRule::new(Token::For,                "for"))
    .add_rule(KeywordRule::new(Token::In,                 "in"))
    .add_rule(KeywordRule::new(Token::While,              "while"))
    .add_rule(KeywordRule::new(Token::Do,                 "do"))
    .add_rule(KeywordRule::new(Token::Continue,           "continue"))
    .add_rule(KeywordRule::new(Token::Break,              "break"))
    .add_rule(KeywordRule::new(Token::Return,             "return"))
    .add_rule(KeywordRule::new(Token::Fun,                "fun"))
    .add_rule(KeywordRule::new(Token::Class,              "class"))
    // .add_rule(KeywordRule::new(Token::Self_,              "self"))
    // .add_rule(KeywordRule::new(Token::Super,              "super"))
    .add_rule(KeywordRule::new(Token::Echo,               "echo"))
    .add_rule(KeywordRule::new(Token::Assert,             "assert"))
    .add_rule(KeywordRule::new(Token::End,                "end"))
    
    // Identifiers and literals
    .add_rule(IdentifierRule::new())
    .add_rule(IntegerLiteralRule::new())
    .add_rule(HexIntegerLiteralRule::new())
    .add_rule(FloatLiteralRule::new())
    .add_rule(StringLiteralRule::new(ESCAPE_SEQUENCES.iter().map(|esc| esc.as_ref())))
    .add_rule(LabelRule::new("::"))
    
}
