#![cfg(test)]

use crate::lexer::{LexerBuilder, Token, TokenMeta, Span};
use crate::lexer::errors::{LexerError, ErrorKind};

#[test]
fn lexer_matches_tokens_1() {
    let source = "foobar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(0), "foo"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "bar"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "baz"))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
        token => {
            token: Token::IntegerLiteral(0),
            span: Span { index: 0, length: 3 },
            newline: true,
        } "foo",
        
        token => {
            token: Token::IntegerLiteral(1),
            span: Span { index: 3, length: 3 },
            newline: false,
        } "bar",
        
        token => {
            token: Token::EOF,
            span: Span { index: 6, length: 0 },
            newline: false,
        } "EOF",
    );
}

#[test]
fn lexer_skips_whitespace() {
    let source = "  foo   bar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "bar"))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
        token => {
            token: Token::IntegerLiteral(1),
            span: Span { index: 2, length: 3 },
            newline: true,
        } "foo",
        
        token => {
            token: Token::IntegerLiteral(2),
            span: Span { index: 8, length: 3 },
            newline: false,
        } "bar",
    );
}

#[test]
fn lexer_tracks_line_numbers() {
    
    let source = " \nfoo \n\n  bar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "bar"))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
        token => {
            token: Token::IntegerLiteral(1),
            span: Span { index: 2, length: 3 },
            newline: true,
        } "foo",
        
        token => {
            token: Token::IntegerLiteral(2),
            span: Span { index: 10, length: 3 },
            newline: true,
        } "bar",
    );
    
}


use crate::lexer::rules::{SingleCharRule, MultiCharRule};


#[test]
fn single_char_rule_matches_chars_and_dont_match_invalid() {
    let source = "a bcd";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(1), 'a'))
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(2), 'b'))
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(3), 'c'))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
        
        token => {
            token: Token::IntegerLiteral(1),
            span: Span { index: 0, length: 1 },
            ..
        } "a",
        
        token => {
            token: Token::IntegerLiteral(2),
            span: Span { index: 2, length: 1 },
            ..
        } "b",
        
        token => {
            token: Token::IntegerLiteral(3),
            span: Span { index: 3, length: 1 },
            ..
        } "c",
        
        error => {
            kind: ErrorKind::NoMatchingRule,
            span: Span { index: 4, length: 1 },
            ..
        } "d",
        
        token => {
            token: Token::EOF,
            span: Span { index: 5, length: 0 },
            ..
        } "EOF",
    );
    
}

#[test]
fn rule_substring_tokens_match_1() {
    let source = "a ab abc";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), 'a'))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "ab"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "abc"))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
        
        token => {
            token: Token::IntegerLiteral(0),
            span: Span { index: 0, length: 1 },
            ..
        } "a",
        
        token => {
            token: Token::IntegerLiteral(1),
            span: Span { index: 2, length: 2 },
            ..
        } "ab",
        
        token => {
            token: Token::IntegerLiteral(2),
            span: Span { index: 5, length: 3 },
            ..
        } "abc",
        
        token => {
            token: Token::EOF,
            span: Span { index: 8, length: 0 },
            ..
        } "EOF"
        
    );

}

#[test]
fn rule_substring_tokens_match_2() {
    let source = "a aab";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), 'a'))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "ab"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "abc"))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
        
        token => {
            token: Token::IntegerLiteral(0),
            span: Span { index: 0, length: 1 },
            ..
        } "a.1",
        
        token => {
            token: Token::IntegerLiteral(0),
            span: Span { index: 2, length: 1 },
            ..
        } "a.2",
        
        token => {
            token: Token::IntegerLiteral(1),
            span: Span { index: 3, length: 2 },
            ..
        } "ab",
        
        token => {
            token: Token::EOF,
            span: Span { index: 5, length: 0 },
            ..
        } "EOF"
        
    );
    
}

#[test]
fn rule_substring_tokens_match_eof() {
    let source = "+\n +";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "+="))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
    
        token => {
            token: Token::IntegerLiteral(0),
            span: Span { index: 0, length: 1 },
            newline: true,
        },
        
        token => {
            token: Token::IntegerLiteral(0),
            span: Span { index: 3, length: 1 },
            newline: true,
        },
        
        token => {
            token: Token::EOF,
            span: Span { index: 4, length: 0 },
            newline: false,
        } "EOF"
    
    );
}

#[test]
fn lexer_test_matches_tokens_2() {
    let source = "and +or ";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "or"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "and"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(3), "+="))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
    
        token => {
            token: Token::IntegerLiteral(2),
            span: Span { index: 0, length: 3 },
            ..
        } "and",
        
        token => {
            token: Token::IntegerLiteral(0),
            span: Span { index: 4, length: 1 },
            ..
        } "+",
        
        token => {
            token: Token::IntegerLiteral(1),
            span: Span { index: 5, length: 2 },
            ..
        } "or",
        
        token => {
            token: Token::EOF,
            span: Span { index: 8, length: 0 },
            ..
        } "EOF"
    
    );
}

#[test]
fn lexer_error_invalid_token() {
    let source = "foo +bar bad baz";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "bar"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(3), "baz"))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
    
        token => {
            token: Token::IntegerLiteral(1),
            span: Span { index: 0, length: 3 },
            ..
        } "foo",
        
        token => {
            token: Token::IntegerLiteral(0),
            span: Span { index: 4, length: 1 },
            ..
        } "+",
        
        token => {
            token: Token::IntegerLiteral(2),
            span: Span { index: 5, length: 3 },
            ..
        } "bar",
        
        error => {
            kind: ErrorKind::NoMatchingRule,
            span: Span { index: 9, length: 3 },
            ..
        } "bad",
        
        token => {
            token: Token::IntegerLiteral(3),
            span: Span { index: 13, length: 3 },
            ..
        } "baz",
        
        token => {
            token: Token::EOF,
            span: Span { index: 16, length: 0 },
            ..
        } "EOF",
    
    );
}
