#![cfg(test)]

use crate::lexer::{LexerBuilder, Token, TokenOut, Span};
use crate::lexer::errors::{LexerError, LexerErrorType};
use crate::lexer::rules::*;


#[test]
fn lexer_matches_tokens_1() {
    let source = "foobar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(ExactRule::new(Token::IntegerLiteral(0), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "bar"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "baz"))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(0),
        location: Span { index: 0, length: 3 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
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
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "bar"))
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
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "bar"))
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
fn single_char_rule_matches_chars_and_dont_match_invalid() {
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
        etype: LexerErrorType::NoMatchingRule,
        location: Span { index: 4, length: 1 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    println!("out: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::EOF,
        location: Span { index: 5, length: 0 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    println!("out: {:?}", out);
}

#[test]
fn rule_substring_tokens_match_1() {
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

#[test]
fn rule_substring_tokens_match_2() {
    let source = "a aab";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), 'a'))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "ab"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "abc"))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(0),
        location: Span { index: 0, length: 1 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(0),
        location: Span { index: 2, length: 1 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 3, length: 2 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::EOF,
        location: Span { index: 5, length: 0 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
}

#[test]
fn rule_substring_tokens_match_eof() {
    let source = "+\n +";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "+="))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(0),
        location: Span { index: 0, length: 1 },
        lineno: 1,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(0),
        location: Span { index: 3, length: 1 },
        lineno: 2,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::EOF,
        location: Span { index: 4, length: 0 },
        lineno: 2,
    }));
}

#[test]
fn lexer_test_matches_tokens_2() {
    let source = "and +or ";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "or"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "and"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(3), "+="))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(2),
        location: Span { index: 0, length: 3 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(0),
        location: Span { index: 4, length: 1 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 5, length: 2 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
    
    let out = lexer.next_token().unwrap();
    assert!(matches!(out, TokenOut {
        token: Token::EOF,
        location: Span { index: 8, length: 0 },
        lineno: 1,
    }), "unexpected output: {:?}", out);
}

#[test]
fn lexer_error_invalid_token() {
    let source = "foo +bar bad baz";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "bar"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(3), "baz"))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 0, length: 3 },
        lineno: 1,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(0),
        location: Span { index: 4, length: 1 },
        lineno: 1,
    }), );
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(2),
        location: Span { index: 5, length: 3 },
        lineno: 1,
    }));
    
    let out = lexer.next_token().unwrap_err();
    println!("out: {:?}", out);
    assert!(matches!(out, LexerError {
        etype: LexerErrorType::NoMatchingRule,
        location: Span { index: 9, length: 3 },
        lineno: 1,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(3),
        location: Span { index: 13, length: 3 },
        lineno: 1,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::EOF,
        location: Span { index: 16, length: 0 },
        lineno: 1,
    }));
}

#[test]
fn lexer_error_ambiguous_match() {
    let source = " + ";
    
    // kind of contrived...
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "+"))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap_err();
    println!("out: {:?}", out);
    assert!(matches!(out, LexerError {
        etype: LexerErrorType::AmbiguousMatch,
        location: Span { index: 1, length: 1 },
        lineno: 1,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::EOF,
        location: Span { index: 3, length: 0 },
        lineno: 1,
    }));
}

#[test]
fn lexer_text_comments() {
    let source = r#"foo #bar
    
      #   #{ 
    
    baz #{ }  #
    
        #{ nest}#
# {
    #
    # baz bad foo boo }# bar
    
    "#;
    
    let mut lexer = LexerBuilder::new()
        .add_rule(ExactRule::new(Token::IntegerLiteral(0), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "bar"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "baz"))
        .add_rule(LineCommentRule::new('#'))
        .add_rule(BlockCommentRule::new("#{", "}#"))
        .build(source.chars());
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(0),
        location: Span { index: 0, length: 3 },
        lineno: 1,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::Comment,
        location: Span { index: 4, length: 5 },
        lineno: 1,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::Comment,
        location: Span { index: 20, length: 8 },
        lineno: 3,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(2),
        location: Span { index: 37, length: 3 },
        lineno: 5,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::Comment,
        location: Span { index: 41, length: 65 },
        lineno: 5,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::IntegerLiteral(1),
        location: Span { index: 107, length: 3 },
        lineno: 10,
    }));
    
    let out = lexer.next_token().unwrap();
    println!("out: {:?}", out);
    assert!(matches!(out, TokenOut {
        token: Token::EOF,
        location: Span { index: 120, length: 0 },
        lineno: 12,
    }));
}