#![cfg(test)]

use crate::lexer::{LexerBuilder, Token, TokenMeta};
use crate::lexer::rules::MultiCharRule;
use crate::lexer::rules::comments::*;

#[test]
fn lexer_test_comments() {
    let source = r#"foo #bar
    
      #   #{ 
    
    baz #{ }  #
    
        #{ nest}#
# {
    #
    # baz bad foo boo }# bar
    
    "#;
    
    let mut lexer = LexerBuilder::new()
        .set_skip_comments(false)  // we're testing them, after all!
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(0), "foo"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "bar"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "baz"))
        .add_rule(LineCommentRule::new('#'))
        .add_rule(BlockCommentRule::new("#{", "}#"))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
    
        token if symbol.len() == 3 => {
            token: Token::IntegerLiteral(0),
            symbol,
            ..
        } "foo",
        
        token if symbol.len() == 5 => {
            token: Token::Comment,
            symbol,
            ..
        } "#bar",
        
        token if symbol.len() == 8 => {
            token: Token::Comment,
            symbol,
            ..
        } "#   #{{",
        
        token if symbol.len() == 3 => {
            token: Token::IntegerLiteral(2),
            symbol,
            ..
        } "baz",
        
        token if symbol.len() == 65 => {
            token: Token::Comment,
            symbol,
            ..
        } "multiline comment block",
    
        token if symbol.len() == 3 => {
            token: Token::IntegerLiteral(1),
            symbol,
            ..
        } "bar",
    
        token if symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
            ..
        } "EOF",
    
    );
}

#[test]
fn lexer_test_skip_comments() {
    let source = r#"foo #bar
    
      #   #{ 
    
    baz #{ }  #
    
        #{ nest}#
# {
    #
    # baz bad foo boo }# bar
    
    "#;
    
    let mut lexer = LexerBuilder::new()
        .set_skip_comments(true)
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(0), "foo"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "bar"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "baz"))
        .add_rule(LineCommentRule::new('#'))
        .add_rule(BlockCommentRule::new("#{", "}#"))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
    
        token if symbol.len() == 3 => {
            token: Token::IntegerLiteral(0),
            symbol,
            ..
        } "foo",
        
        token if symbol.len() == 3 => {
            token: Token::IntegerLiteral(2),
            symbol,
            ..
        } "baz",
        
        token if symbol.len() == 3 => {
            token: Token::IntegerLiteral(1),
            symbol,
            ..
        } "bar",
        
        token if symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
            ..
        } "EOF",
    
    );
}