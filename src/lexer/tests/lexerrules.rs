#![cfg(test)]

use crate::debug::DebugSymbol;
use crate::lexer::{LexerBuilder, Token, TokenMeta};
use crate::lexer::errors::{LexerError, ErrorKind};
use crate::lexer::tests::ErrorData;

#[test]
fn lexer_matches_tokens_1() {
    let source = "foobar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(0), "foo"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(1), "bar"))
        .add_rule(MultiCharRule::new(Token::IntegerLiteral(2), "baz"))
        .build_once(source.chars().map(|c| Ok(c)));
    
    assert_token_sequence!(lexer,
        token if symbol.start() == 0 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(0),
            symbol,
            newline: true,
        } "foo",
        
        token if symbol.start() == 3 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(1),
            symbol,
            newline: false,
        } "bar",
        
        token if symbol.start() == 6 && symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
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
        token if symbol.start() == 2 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(1),
            symbol,
            newline: true,
        } "foo",
        
        token if symbol.start() == 8 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(2),
            symbol,
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
        token if symbol.start() == 2 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(1),
            symbol,
            newline: true,
        } "foo",
        
        token if symbol.start() == 10 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(2),
            symbol,
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
        
        token if symbol.start() == 0 && symbol.len() == 1 => {
            token: Token::IntegerLiteral(1),
            symbol,
            ..
        } "a",
        
        token if symbol.start() == 2 && symbol.len() == 1 => {
            token: Token::IntegerLiteral(2),
            symbol,
            ..
        } "b",
        
        token if symbol.start() == 3 && symbol.len() == 1 => {
            token: Token::IntegerLiteral(3),
            symbol,
            ..
        } "c",
        
        error if symbol.start() == 4 && symbol.len() == 1 => {
            kind: ErrorKind::NoMatchingRule,
            symbol,
            ..
        } "d",
        
        token if symbol.start() == 5 && symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
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
        
        token if symbol.start() == 0 && symbol.len() == 1 => {
            token: Token::IntegerLiteral(0),
            symbol,
            ..
        } "a",
        
        token if symbol.start() == 2 && symbol.len() == 2 => {
            token: Token::IntegerLiteral(1),
            symbol,
            ..
        } "ab",
        
        token if symbol.start() == 5 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(2),
            symbol,
            ..
        } "abc",
        
        token if symbol.start() == 8 && symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
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
        
        token if symbol.start() == 0 && symbol.len() == 1 => {
            token: Token::IntegerLiteral(0),
            symbol,
            ..
        } "a.1",
        
        token if symbol.start() == 2 && symbol.len() == 1 => {
            token: Token::IntegerLiteral(0),
            symbol,
            ..
        } "a.2",
        
        token if symbol.start() == 3 && symbol.len() == 2 => {
            token: Token::IntegerLiteral(1),
            symbol,
            ..
        } "ab",
        
        token if symbol.start() == 5 && symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
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
    
        token if symbol.start() == 0 && symbol.len() == 1 => {
            token: Token::IntegerLiteral(0),
            symbol,
            newline: true,
        },
        
        token if symbol.start() == 3 && symbol.len() == 1 => {
            token: Token::IntegerLiteral(0),
            symbol,
            newline: true,
        },
        
        token if symbol.start() == 4 && symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
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
    
        token if symbol.start() == 0 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(2),
            symbol,
            ..
        } "and",
        
        token if symbol.start() == 4 && symbol.len() == 1 => {
            token: Token::IntegerLiteral(0),
            symbol,
            ..
        } "+",
        
        token if symbol.start() == 5 && symbol.len() == 2 => {
            token: Token::IntegerLiteral(1),
            symbol,
            ..
        } "or",
        
        token if symbol.start() == 8 && symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
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
    
        token if symbol.start() == 0 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(1),
            symbol,
            ..
        } "foo",
        
        token if symbol.start() == 4 && symbol.len() == 1 => {
            token: Token::IntegerLiteral(0),
            symbol,
            ..
        } "+",
        
        token if symbol.start() == 5 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(2),
            symbol,
            ..
        } "bar",
        
        error if symbol.start() == 9 && symbol.len() == 3 => {
            kind: ErrorKind::NoMatchingRule,
            symbol,
            ..
        } "bad",
        
        token if symbol.start() == 13 && symbol.len() == 3 => {
            token: Token::IntegerLiteral(3),
            symbol,
            ..
        } "baz",
        
        token if symbol.start() == 16 && symbol.len() == 0 => {
            token: Token::EOF,
            symbol,
            ..
        } "EOF",
    
    );
}
