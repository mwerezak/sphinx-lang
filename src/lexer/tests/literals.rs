#![cfg(test)]

use crate::lexer::{LexerBuilder, Token, TokenMeta, LexerError, LexerErrorType, Span};
use crate::lexer::rules::SingleCharRule;
use crate::lexer::rules::literals::*;


#[test]
fn lexer_test_identifiers() {
    let source = r#"
        valid _also asd2_32df_s3
        
        both+valid2
        
        0not _0valid 
        
    "#;
    
    let mut lexer = LexerBuilder::new()
        .add_rule(IdentifierRule::new())
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .build(source.chars());
    
    // let token = lexer.next_token().unwrap();
    // println!("{:?}", token);
    // assert!(matches!(token, 
    //     TokenMeta {
    //         token: Token::Identifier(ref s),
    //         ..
    //     } if s == "isvalid"
    // ));
    
    assert_token_sequence!(lexer,
    
        token if s == "valid" => {
            token: Token::Identifier(ref s),
            location: Span { length: 5, .. },
            lineno: 2,
        } "valid",

        token if s == "_also" => {
            token: Token::Identifier(ref s),
            location: Span { length: 5, .. },
            lineno: 2,
        } "_also",

        token if s == "asd2_32df_s3" => {
            token: Token::Identifier(ref s),
            location: Span { length: 12, .. },
            lineno: 2,
        } "asd2_32df_s3",

        token if s == "both" => {
            token: Token::Identifier(ref s),
            location: Span { length: 4, .. },
            lineno: 4,
        } "both",
        
        token => {
            token: Token::IntegerLiteral(0),
            location: Span { length: 1, .. },
            lineno: 4,
        } "+",
        
        token if s == "valid2" => {
            token: Token::Identifier(ref s),
            location: Span { length: 6, .. },
            lineno: 4,
        } "valid2",
        
        error => {
            etype: LexerErrorType::NoMatchingRule,
            location: Span { length: 1, .. },
            lineno: 6,
        } "0not",

        token if s == "_0valid" => {
            token: Token::Identifier(ref s),
            location: Span { length: 7, .. },
            lineno: 6,
        } "_0valid",

        token => {
            token: Token::EOF,
            location: Span { length: 0, .. },
            lineno: 8,
        } "EOF",
    
    );

}