#![cfg(test)]

use crate::lexer::{LexerBuilder, Token, TokenMeta, Span};
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
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token => {
            token: Token::IntegerLiteral(0),
            span: Span { length: 3, lineno: 1, .. },
        } "foo",
        
        token => {
            token: Token::Comment,
            span: Span { length: 5, lineno: 1, .. },
        } "#bar",
        
        token => {
            token: Token::Comment,
            span: Span { length: 8, lineno: 3, .. },
        } "#   #{{",
        
        token => {
            token: Token::IntegerLiteral(2),
            span: Span { length: 3, lineno: 5, .. },
        } "baz",
        
        token => {
            token: Token::Comment,
            span: Span { length: 65, lineno: 5, .. },
        } "multiline comment block",
    
        token => {
            token: Token::IntegerLiteral(1),
            span: Span { length: 3, lineno: 10, .. },
        } "bar",
    
        token => {
            token: Token::EOF,
            span: Span { length: 0, lineno: 12, .. },
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
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token => {
            token: Token::IntegerLiteral(0),
            span: Span { length: 3, lineno: 1, .. },
        } "foo",
        
        token => {
            token: Token::IntegerLiteral(2),
            span: Span { length: 3, lineno: 5, .. },
        } "baz",
        
        token => {
            token: Token::IntegerLiteral(1),
            span: Span { length: 3, lineno: 10, .. },
        } "bar",
        
        token => {
            token: Token::EOF,
            span: Span { length: 0, lineno: 12, .. },
        } "EOF",
    
    );
}