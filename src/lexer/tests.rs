#![cfg(test)]

use crate::lexer::{LexerBuilder, LexerRule, LexerMatch, Token, TokenOut, Span, LexerError};

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
fn lexer_matches_tokens() {
    let source = "foobar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(TestRule::new("foo", Token::IntegerLiteral(1)))
        .add_rule(TestRule::new("bar", Token::IntegerLiteral(2)))
        .add_rule(TestRule::new("baz", Token::IntegerLiteral(3)))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 0, length: 3 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(2),
        location: Span { index: 3, length: 3 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::EOF,
        location: Span { index: 6, length: 0 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
}

#[test]
fn lexer_skips_whitespace() {
    let source = "  foo   bar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(TestRule::new("foo", Token::IntegerLiteral(1)))
        .add_rule(TestRule::new("bar", Token::IntegerLiteral(2)))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 2, length: 3 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(2),
        location: Span { index: 8, length: 3 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
}

#[test]
fn lexer_tracks_line_numbers() {
    let source = " \nfoo \n\n  bar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(TestRule::new("foo", Token::IntegerLiteral(1)))
        .add_rule(TestRule::new("bar", Token::IntegerLiteral(2)))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 2, length: 3 },
        lineno: 2,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(2),
        location: Span { index: 10, length: 3 },
        lineno: 4,
    }), "unexpected output: {:?}", out);
}


use crate::lexer::rules::{SingleCharRule, ExactRule};


#[test]
fn single_char_rule_matches_chars() {
    let source = "a bcd";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(1), 'a'))
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(2), 'b'))
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(3), 'c'))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 0, length: 1 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    println!("out: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(2),
        location: Span { index: 2, length: 1 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    println!("out: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(3),
        location: Span { index: 3, length: 1 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    println!("out: {:?}", out);
    
    let out = lexer.next_token().unwrap_err();
    assert!(matches!(out, LexerError {
        location: Span { index: 4, length: 0 },
        lineno: 1,
        ..
    }), "unexpected output: {:?}", out);
    println!("out: {:?}", out);
}

#[test]
fn rule_substrings_match_correctly() {
    let source = "a ab abc";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), 'a'))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "ab"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "abc"))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(0),
        location: Span { index: 0, length: 1 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 2, length: 2 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(2),
        location: Span { index: 5, length: 3 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::EOF,
        location: Span { index: 8, length: 0 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
}