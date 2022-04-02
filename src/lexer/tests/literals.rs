#![cfg(test)]

use crate::lexer::{LexerBuilder, Token, TokenMeta, LexerError, ErrorKind, DebugSymbol};
use crate::lexer::rules::SingleCharRule;
use crate::lexer::rules::literals::*;
use crate::lexer::rules::keywords::KeywordRule;
use crate::lexer::tests::ErrorData;

#[test]
fn lexer_test_identifiers() {
    let source = r#"
        valid _also asd2_32df_s3
        
        both+valid2
        
        0no _0valid 
        
    "#;
    
    let mut lexer = LexerBuilder::new()
        .add_rule(IdentifierRule::new())
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .build_once(source.chars().map(|c| Ok(c)));
    
    
    assert_token_sequence!(lexer,
    
        token if s == "valid" && symbol.len() == 5 => {
            token: Token::Identifier(s),
            symbol,
            ..
        } "valid",

        token if s == "_also" && symbol.len() == 5 => {
            token: Token::Identifier(s),
            symbol,
            ..
        } "_also",

        token if s == "asd2_32df_s3" && symbol.len() == 12 => {
            token: Token::Identifier(s),
            symbol,
            ..
        } "asd2_32df_s3",
        
        token if s == "both" && symbol.len() == 4 => {
            token: Token::Identifier(s),
            symbol,
            ..
        } "both",
        
        token if symbol.len() == 1 => {
            token: Token::IntegerLiteral(0),
            symbol,
            ..
        } "+",
        
        token if s == "valid2" && symbol.len() == 6 => {
            token: Token::Identifier(s),
            symbol,
            ..
        } "valid2",
        
        error if symbol.len() == 1 => {
            kind: ErrorKind::NoMatchingRule,
            symbol,
            ..
        } "0no - 0",
        
        error if symbol.len() == 1 => {
            kind: ErrorKind::NoMatchingRule,
            symbol,
            ..
        } "0no - n",

        error if symbol.len() == 1 => {
            kind: ErrorKind::NoMatchingRule,
            symbol,
            ..
        } "0no - o",

        token if s == "_0valid" && symbol.len() == 7 => {
            token: Token::Identifier(s),
            symbol,
            ..
        } "_0valid",

        token if symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
            ..
        } "EOF",
    
    );

}

#[test]
fn lexer_test_keywords_and_identifiers() {
    let source = " k   _k  9k k9 ";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(KeywordRule::new(Token::Fun, "k"))
        .add_rule(IdentifierRule::new())
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
    
        token if symbol.len() == 1 => {
            token: Token::Fun,
            symbol,
            ..
        } "k",
        
        token if s == "_k" && symbol.len() == 2 => {
            token: Token::Identifier(s),
            symbol,
            ..
        } "_k",
        
        error if symbol.len() == 1 => {
            kind: ErrorKind::NoMatchingRule,
            symbol,
            ..
        } "9k.1",
        
        error if symbol.len() == 1 => {
            kind: ErrorKind::NoMatchingRule,
            symbol,
            ..
        } "9k.2",
        
        token if s == "k9" && symbol.len() == 2 => {
            token: Token::Identifier(s),
            symbol,
            ..
        } "k9",
    );

}

#[test]
fn lexer_test_keyword_at_eof() {
    let source = " k";
    let mut lexer = LexerBuilder::new()
        .add_rule(KeywordRule::new(Token::Fun, "k"))
        .add_rule(IdentifierRule::new())
        .build_once(source.chars().map(|c| Ok(c)));
        
    assert_token_sequence!(lexer,
        
        token if symbol.start() == 1 && symbol.len() == 1 => {
            token: Token::Fun,
            symbol,
            ..
        } "k",
        
        token if symbol.start() == 2 && symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
            ..
        } "eof",
        
    );
}


#[test]
fn lexer_test_integer_literals() {
    let source = " 01123 xA  0xFACE ";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(IntegerLiteralRule::new())
        .add_rule(HexIntegerLiteralRule::new())
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
        
        token if n == 1123 && symbol.len() == 5 => {
            token: Token::IntegerLiteral(n),
            symbol,
            ..
        } "01123",
        
        error if symbol.len() == 1 => {
            kind: ErrorKind::NoMatchingRule,
            symbol,
            ..
        } "x",
        
        error if symbol.len() == 1 => {
            kind: ErrorKind::NoMatchingRule,
            symbol,
            ..
        } "A",
        
        token if n == 0xFACE && symbol.len() == 6 => {
            token: Token::IntegerLiteral(n),
            symbol,
            ..
        } "0xFACE",
        
    );
}