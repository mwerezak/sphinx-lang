#![cfg(test)]

use crate::lexer::{LexerBuilder, Token, TokenMeta, Span};
use crate::lexer::errors::{LexerError, LexerErrorType};

#[test]
fn lexer_matches_tokens_1() {
    let source = "foobar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(0), "foo"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "bar"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "baz"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        token => {
            token: Token::IntegerLiteral(0),
            location: Span { index: 0, length: 3 },
            lineno: 1,
        } "foo",
        
        token => {
            token: Token::IntegerLiteral(1),
            location: Span { index: 3, length: 3 },
            lineno: 1,
        } "bar",
        
        token => {
            token: Token::EOF,
            location: Span { index: 6, length: 0 },
            lineno: 1,
        } "EOF",
    );
}

#[test]
fn lexer_skips_whitespace() {
    let source = "  foo   bar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "bar"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        token => {
            token: Token::IntegerLiteral(1),
            location: Span { index: 2, length: 3 },
            lineno: 1,
        } "foo",
        
        token => {
            token: Token::IntegerLiteral(2),
            location: Span { index: 8, length: 3 },
            lineno: 1,
        } "bar",
    );
}

#[test]
fn lexer_tracks_line_numbers() {
    
    let source = " \nfoo \n\n  bar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "bar"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        
        token => {
            token: Token::IntegerLiteral(1),
            location: Span { index: 2, length: 3 },
            lineno: 2,
        } "foo",
        
        token => {
            token: Token::IntegerLiteral(2),
            location: Span { index: 10, length: 3 },
            lineno: 4,
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
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        
        token => {
            token: Token::IntegerLiteral(1),
            location: Span { index: 0, length: 1 },
            lineno: 1,
        } "a",
        
        token => {
            token: Token::IntegerLiteral(2),
            location: Span { index: 2, length: 1 },
            lineno: 1,
        } "b",
        
        token => {
            token: Token::IntegerLiteral(3),
            location: Span { index: 3, length: 1 },
            lineno: 1,
        } "c",
        
        error => {
            etype: LexerErrorType::NoMatchingRule,
            location: Span { index: 4, length: 1 },
            lineno: 1,
        } "d",
        
        token => {
            token: Token::EOF,
            location: Span { index: 5, length: 0 },
            lineno: 1,
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
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        
        token => {
            token: Token::IntegerLiteral(0),
            location: Span { index: 0, length: 1 },
            lineno: 1,
        } "a",
        
        token => {
            token: Token::IntegerLiteral(1),
            location: Span { index: 2, length: 2 },
            lineno: 1,
        } "ab",
        
        token => {
            token: Token::IntegerLiteral(2),
            location: Span { index: 5, length: 3 },
            lineno: 1,
        } "abc",
        
        token => {
            token: Token::EOF,
            location: Span { index: 8, length: 0 },
            lineno: 1,
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
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        
        token => {
            token: Token::IntegerLiteral(0),
            location: Span { index: 0, length: 1 },
            lineno: 1,
        } "a.1",
        
        token => {
            token: Token::IntegerLiteral(0),
            location: Span { index: 2, length: 1 },
            lineno: 1,
        } "a.2",
        
        token => {
            token: Token::IntegerLiteral(1),
            location: Span { index: 3, length: 2 },
            lineno: 1,
        } "ab",
        
        token => {
            token: Token::EOF,
            location: Span { index: 5, length: 0 },
            lineno: 1,
        } "EOF"
        
    );
    
}

#[test]
fn rule_substring_tokens_match_eof() {
    let source = "+\n +";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "+="))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token => {
            token: Token::IntegerLiteral(0),
            location: Span { index: 0, length: 1 },
            lineno: 1,
        },
        
        token => {
            token: Token::IntegerLiteral(0),
            location: Span { index: 3, length: 1 },
            lineno: 2,
        },
        
        token => {
            token: Token::EOF,
            location: Span { index: 4, length: 0 },
            lineno: 2,
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
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token => {
            token: Token::IntegerLiteral(2),
            location: Span { index: 0, length: 3 },
            lineno: 1,
        } "and",
        
        token => {
            token: Token::IntegerLiteral(0),
            location: Span { index: 4, length: 1 },
            lineno: 1,
        } "+",
        
        token => {
            token: Token::IntegerLiteral(1),
            location: Span { index: 5, length: 2 },
            lineno: 1,
        } "or",
        
        token => {
            token: Token::EOF,
            location: Span { index: 8, length: 0 },
            lineno: 1,
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
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token => {
            token: Token::IntegerLiteral(1),
            location: Span { index: 0, length: 3 },
            lineno: 1,
        } "foo",
        
        token => {
            token: Token::IntegerLiteral(0),
            location: Span { index: 4, length: 1 },
            lineno: 1,
        } "+",
        
        token => {
            token: Token::IntegerLiteral(2),
            location: Span { index: 5, length: 3 },
            lineno: 1,
        } "bar",
        
        error => {
            etype: LexerErrorType::NoMatchingRule,
            location: Span { index: 9, length: 3 },
            lineno: 1,
        } "bad",
        
        token => {
            token: Token::IntegerLiteral(3),
            location: Span { index: 13, length: 3 },
            lineno: 1,
        } "baz",
        
        token => {
            token: Token::EOF,
            location: Span { index: 16, length: 0 },
            lineno: 1,
        } "EOF",
    
    );
}
