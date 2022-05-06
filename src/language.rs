use string_interner::symbol::SymbolUsize;
use once_cell::sync::OnceCell;

use crate::lexer::{LexerBuilder, Token};
use crate::lexer::rules::{SingleCharRule, MultiCharRule};
use crate::lexer::rules::keywords::KeywordRule;
use crate::lexer::rules::literals::*;
use crate::lexer::rules::literals::string::*;


// Internal representation for integers
#[cfg(target_pointer_width = "32")]
pub type IntType = i32;
#[cfg(target_pointer_width = "64")]
pub type IntType = i64;


// Internal representation for floats
#[cfg(target_pointer_width = "32")]
pub type FloatType = f32;
#[cfg(target_pointer_width = "64")]
pub type FloatType = f64;


// Interned string symbol representation
pub type InternSymbol = SymbolUsize;


// Comment markers
pub static COMMENT_CHAR: char = '#';
pub static NESTED_COMMENT_START: &str = "#{";
pub static NESTED_COMMENT_END:   &str = "}#";


// Variable access modes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Access {
    ReadOnly,
    ReadWrite,
}

impl Access {
    pub fn can_write(&self) -> bool {
        matches!(self, Self::ReadWrite)
    }
}


// String literal escape sequences
static ESCAPE_SEQUENCES: OnceCell<Vec<Box<dyn EscapeSequence>>> = OnceCell::new();

pub fn all_escape_sequences() -> impl Iterator<Item=&'static dyn EscapeSequence> {
    ESCAPE_SEQUENCES.get_or_init(|| {
            
            let escapes: Vec<Box<dyn EscapeSequence>> = vec![
                
                Box::new(CharMapEscape::new('0', "\x00")),
                Box::new(CharMapEscape::new('\\', "\\")),
                Box::new(CharMapEscape::new('\'', "\'")),
                Box::new(CharMapEscape::new('\"', "\"")),
                
                Box::new(CharMapEscape::new('t', "\t")),
                Box::new(CharMapEscape::new('n', "\n")),
                Box::new(CharMapEscape::new('r', "\r")),
                Box::new(HexByteEscape::new()),
            ];
            
            escapes
            
        })
        .iter().map(|esc| &**esc)
}


// Tokens
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
    .add_rule(MultiCharRule::new(Token::OpExp,            "**"))
    
    .add_rule(SingleCharRule::new(Token::OpAdd,           '+'))
    .add_rule(SingleCharRule::new(Token::OpSub,           '-'))
    .add_rule(SingleCharRule::new(Token::OpMul,           '*'))
    .add_rule(SingleCharRule::new(Token::OpDiv,           '/'))
    .add_rule(SingleCharRule::new(Token::OpMod,           '%'))
    .add_rule(SingleCharRule::new(Token::OpInv,           '~'))
    .add_rule(SingleCharRule::new(Token::OpAnd,           '&'))
    .add_rule(SingleCharRule::new(Token::OpOr,            '|'))
    .add_rule(SingleCharRule::new(Token::OpXor,           '^'))
    
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
    .add_rule(KeywordRule::new(Token::Local,              "local"))
    .add_rule(KeywordRule::new(Token::NonLocal,           "nonlocal"))
    .add_rule(KeywordRule::new(Token::Del,                "del"))
    .add_rule(KeywordRule::new(Token::Begin,              "begin"))
    .add_rule(KeywordRule::new(Token::If,                 "if"))
    .add_rule(KeywordRule::new(Token::Then,               "then"))
    .add_rule(KeywordRule::new(Token::Elif,               "elif"))
    .add_rule(KeywordRule::new(Token::Else,               "else"))
    .add_rule(KeywordRule::new(Token::Loop,               "loop"))
    .add_rule(KeywordRule::new(Token::While,              "while"))
    .add_rule(KeywordRule::new(Token::For,                "for"))
    .add_rule(KeywordRule::new(Token::In,                 "in"))
    .add_rule(KeywordRule::new(Token::Do,                 "do"))
    .add_rule(KeywordRule::new(Token::Continue,           "continue"))
    .add_rule(KeywordRule::new(Token::Break,              "break"))
    .add_rule(KeywordRule::new(Token::Return,             "return"))
    .add_rule(KeywordRule::new(Token::Fun,                "fun"))
    .add_rule(KeywordRule::new(Token::Class,              "class"))
    // .add_rule(KeywordRule::new(Token::Self_,              "self"))
    // .add_rule(KeywordRule::new(Token::Super,              "super"))
    // .add_rule(KeywordRule::new(Token::Echo,               "echo"))
    .add_rule(KeywordRule::new(Token::Assert,             "assert"))
    .add_rule(KeywordRule::new(Token::End,                "end"))
    
    // Identifiers and literals
    .add_rule(IdentifierRule::new())
    .add_rule(IntegerLiteralRule::new())
    .add_rule(PrefixedIntegerLiteralRule::new("0x", 16))
    .add_rule(PrefixedIntegerLiteralRule::new("0o", 8))
    .add_rule(PrefixedIntegerLiteralRule::new("0b", 2))
    .add_rule(FloatLiteralRule::new())
    .add_rule(StringLiteralRule::new(all_escape_sequences()))
    .add_rule(LabelRule::new("::"))
    
}
