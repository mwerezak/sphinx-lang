#![cfg(test)]

use crate::lexer::{LexerBuilder, LexerRule, LexerMatch, Token, TokenOut, Span};

struct TestRule {
    buf: String,
    target: String,
    result: Token,
}

impl TestRule {
    fn new(target: &str, result: Token) -> Self {
        TestRule {
            result,
            target: String::from(target),
            buf: String::new(),
        }
    }
}

impl LexerRule for TestRule {
    fn current_state(&self) -> LexerMatch {
        if self.buf == self.target {
            LexerMatch::CompleteMatch
        } else if self.target.starts_with(&self.buf) {
            LexerMatch::IncompleteMatch
        } else {
            LexerMatch::NoMatch
        }
    }
    
    fn feed(&mut self, ch: char) -> LexerMatch {
        self.buf.push(ch);
        return self.current_state();
    }
    
    fn try_match(&mut self, ch: char) -> LexerMatch {
        self.buf.push(ch);
        let match_result = self.current_state();
        if let LexerMatch::NoMatch = match_result {
            self.buf.pop();
        }
        return match_result;
    }
    
    fn reset(&mut self) {
        self.buf.clear();
    }
    
    fn get_token(&self) -> Option<Token> {
        match self.current_state() {
            LexerMatch::NoMatch => None,
            _ => Some(self.result.clone()),
        }
    }
}

#[test]
fn test_lexer_matches_tokens() {
    let source = "foobar";
    
    let rules = vec![
        TestRule::new("foo", Token::IntegerLiteral(1)),
        TestRule::new("bar", Token::IntegerLiteral(2)),
        TestRule::new("baz", Token::IntegerLiteral(3)),
    ];
    
    let mut builder = LexerBuilder::new();
    for rule in rules.into_iter() {
        builder.add_rule(rule);
    }
    let mut lexer = builder.build(source.chars());
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 0, length: 3 },
        ..
    }), "{:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(2),
        location: Span { index: 3, length: 3 },
        ..
    }), "{:?}", out);
}

#[test]
fn test_lexer_skips_whitespace() {
    let source = "  foo   bar";
    
    let rules = vec![
        TestRule::new("foo", Token::IntegerLiteral(1)),
        TestRule::new("bar", Token::IntegerLiteral(2)),
    ];
    
    let mut builder = LexerBuilder::new();
    for rule in rules.into_iter() {
        builder.add_rule(rule);
    }
    let mut lexer = builder.build(source.chars());
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 2, length: 3 },
        ..
    }), "{:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(2),
        location: Span { index: 8, length: 3 },
        ..
    }), "{:?}", out);
}