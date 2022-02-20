#![cfg(test)]

use crate::lexer::{LexerBuilder, Token, TokenMeta, LexerError, LexerErrorKind, Span};
use crate::lexer::rules::SingleCharRule;
use crate::lexer::rules::literals::*;


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
        .build(source.chars());
    
    
    assert_token_sequence!(lexer,
    
        token if s == "valid" => {
            token: Token::Identifier(s),
            location: Span { length: 5, .. },
            ..
        } "valid",

        token if s == "_also" => {
            token: Token::Identifier(s),
            location: Span { length: 5, .. },
            ..
        } "_also",

        token if s == "asd2_32df_s3" => {
            token: Token::Identifier(s),
            location: Span { length: 12, .. },
            ..
        } "asd2_32df_s3",
        
        token if s == "both" => {
            token: Token::Identifier(s),
            location: Span { length: 4, .. },
            ..
        } "both",
        
        token => {
            token: Token::IntegerLiteral(0),
            location: Span { length: 1, .. },
            ..
        } "+",
        
        token if s == "valid2" => {
            token: Token::Identifier(s),
            location: Span { length: 6, .. },
            ..
        } "valid2",
        
        error => {
            kind: LexerErrorKind::NoMatchingRule,
            location: Span { length: 1, .. },
            ..
        } "0no - 0",
        
        error => {
            kind: LexerErrorKind::NoMatchingRule,
            location: Span { length: 1, .. },
            ..
        } "0no - n",

        error => {
            kind: LexerErrorKind::NoMatchingRule,
            location: Span { length: 1, .. },
            ..
        } "0no - o",

        token if s == "_0valid" => {
            token: Token::Identifier(s),
            location: Span { length: 7, .. },
            ..
        } "_0valid",

        token => {
            token: Token::EOF,
            location: Span { length: 0, .. },
            ..
        } "EOF",
    
    );

}

use crate::lexer::rules::keywords::KeywordRule;

#[test]
fn lexer_test_keywords_and_identifiers() {
    let source = " k   _k  9k k9 ";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(KeywordRule::new(Token::Fun, "k"))
        .add_rule(IdentifierRule::new())
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token => {
            token: Token::Fun,
            location: Span { length: 1, .. },
            ..
        } "k",
        
        token if s == "_k" => {
            token: Token::Identifier(s),
            location: Span { length: 2, .. },
            ..
        } "_k",
        
        error => {
            kind: LexerErrorKind::NoMatchingRule,
            location: Span { length: 1, .. },
            ..
        } "9k.1",
        
        error => {
            kind: LexerErrorKind::NoMatchingRule,
            location: Span { length: 1, .. },
            ..
        } "9k.2",
        
        token if s == "k9" => {
            token: Token::Identifier(s),
            location: Span { length: 2, .. },
            ..
        } "k9",
    );

}


#[test]
fn lexer_test_integer_literals() {
    let source = " 01123 xA  0xFACE ";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(IntegerLiteralRule::new())
        .add_rule(HexIntegerLiteralRule::new())
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        
        token if n == 1123 => {
            token: Token::IntegerLiteral(n),
            location: Span { length: 5, .. },
            ..
        } "01123",
        
        error => {
            kind: LexerErrorKind::NoMatchingRule,
            location: Span { length: 1, .. },
            ..
        } "x",
        
        error => {
            kind: LexerErrorKind::NoMatchingRule,
            location: Span { length: 1, .. },
            ..
        } "A",
        
        token if n == 0xFACE => {
            token: Token::IntegerLiteral(n),
            location: Span { length: 6, .. },
            ..
        } "0xFACE",
        
    );
}