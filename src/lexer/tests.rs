#![cfg(test)]

use super::{LexerBuilder, Token, TokenOut, Span};
use super::errors::{LexerError, LexerErrorType};
use super::rules::comments::*;

macro_rules! assert_next_token {
    
    // assert_next_token!(<lexer>, token { <match body> }, "failure message")
    ( $lexer:expr, token $token_body:tt $(, $msg:expr )? ) => {
        
        let token = $lexer.next_token().unwrap();
        
        println!("token: {:?}", token);
        assert!(matches!(token, TokenOut $token_body) $(, $msg )?)
    };
    
    // assert_next_token!(<lexer>, error { <match body> }, "failure message")
    ( $lexer:expr, error $error_body:tt $(, $msg:expr )? ) => {
        
        let error = $lexer.next_token().unwrap_err();
        
        println!("error: {:?}", error);
        assert!(matches!(error, LexerError $error_body)  $(, $msg )?)
    };
}

// assert_token_sequence!(<lexer>,
//      <list of items for assert_next_token!()>
// );
macro_rules! assert_token_sequence {
    ( $lexer:expr, $( $item:tt  $body:tt $( $msg:expr )? ),* $( , )? ) => {
        
        $( assert_next_token!($lexer, $item $body $(, $msg )? ); )*
        
    };
}

#[test]
fn lexer_matches_tokens_1() {
    let source = "foobar";
    
    let mut lexer = LexerBuilder::new()
        .add_rule(ExactRule::new(Token::IntegerLiteral(0), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "bar"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "baz"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        token {
            token: Token::IntegerLiteral(0),
            location: Span { index: 0, length: 3 },
            lineno: 1,
        } "foo",
        
        token {
            token: Token::IntegerLiteral(1),
            location: Span { index: 3, length: 3 },
            lineno: 1,
        } "bar",
        
        token {
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
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "bar"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        token {
            token: Token::IntegerLiteral(1),
            location: Span { index: 2, length: 3 },
            lineno: 1,
        } "foo",
        
        token {
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
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "bar"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        
        token {
            token: Token::IntegerLiteral(1),
            location: Span { index: 2, length: 3 },
            lineno: 2,
        } "foo",
        
        token {
            token: Token::IntegerLiteral(2),
            location: Span { index: 10, length: 3 },
            lineno: 4,
        } "bar",
    );
    
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
    
    assert_token_sequence!(lexer,
        
        token {
            token: Token::IntegerLiteral(1),
            location: Span { index: 0, length: 1 },
            lineno: 1,
        } "a",
        
        token {
            token: Token::IntegerLiteral(2),
            location: Span { index: 2, length: 1 },
            lineno: 1,
        } "b",
        
        token {
            token: Token::IntegerLiteral(3),
            location: Span { index: 3, length: 1 },
            lineno: 1,
        } "c",
        
        error {
            etype: LexerErrorType::NoMatchingRule,
            location: Span { index: 4, length: 1 },
            lineno: 1,
        } "d",
        
        token {
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
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "ab"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "abc"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        
        token {
            token: Token::IntegerLiteral(0),
            location: Span { index: 0, length: 1 },
            lineno: 1,
        } "a",
        
        token {
            token: Token::IntegerLiteral(1),
            location: Span { index: 2, length: 2 },
            lineno: 1,
        } "ab",
        
        token {
            token: Token::IntegerLiteral(2),
            location: Span { index: 5, length: 3 },
            lineno: 1,
        } "abc",
        
        token {
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
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "ab"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "abc"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
        
        token {
            token: Token::IntegerLiteral(0),
            location: Span { index: 0, length: 1 },
            lineno: 1,
        } "a.1",
        
        token {
            token: Token::IntegerLiteral(0),
            location: Span { index: 2, length: 1 },
            lineno: 1,
        } "a.2",
        
        token {
            token: Token::IntegerLiteral(1),
            location: Span { index: 3, length: 2 },
            lineno: 1,
        } "ab",
        
        token {
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
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "+="))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token {
            token: Token::IntegerLiteral(0),
            location: Span { index: 0, length: 1 },
            lineno: 1,
        },
        
        token {
            token: Token::IntegerLiteral(0),
            location: Span { index: 3, length: 1 },
            lineno: 2,
        },
        
        token {
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
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "or"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "and"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(3), "+="))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token {
            token: Token::IntegerLiteral(2),
            location: Span { index: 0, length: 3 },
            lineno: 1,
        } "and",
        
        token {
            token: Token::IntegerLiteral(0),
            location: Span { index: 4, length: 1 },
            lineno: 1,
        } "+",
        
        token {
            token: Token::IntegerLiteral(1),
            location: Span { index: 5, length: 2 },
            lineno: 1,
        } "or",
        
        token {
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
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "bar"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(3), "baz"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token {
            token: Token::IntegerLiteral(1),
            location: Span { index: 0, length: 3 },
            lineno: 1,
        } "foo",
        
        token {
            token: Token::IntegerLiteral(0),
            location: Span { index: 4, length: 1 },
            lineno: 1,
        } "+",
        
        token {
            token: Token::IntegerLiteral(2),
            location: Span { index: 5, length: 3 },
            lineno: 1,
        } "bar",
        
        error {
            etype: LexerErrorType::NoMatchingRule,
            location: Span { index: 9, length: 3 },
            lineno: 1,
        } "bad",
        
        token {
            token: Token::IntegerLiteral(3),
            location: Span { index: 13, length: 3 },
            lineno: 1,
        } "baz",
        
        token {
            token: Token::EOF,
            location: Span { index: 16, length: 0 },
            lineno: 1,
        } "EOF",
    
    );
}

#[test]
fn lexer_error_ambiguous_match() {
    let source = " + ";
    
    // kind of contrived...
    let mut lexer = LexerBuilder::new()
        .add_rule(SingleCharRule::new(Token::IntegerLiteral(0), '+'))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "+"))
        .build(source.chars());
    
    assert_next_token!(lexer, error {
        etype: LexerErrorType::AmbiguousMatch,
        location: Span { index: 1, length: 1 },
        lineno: 1,
    });

}

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
        .add_rule(ExactRule::new(Token::IntegerLiteral(0), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "bar"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "baz"))
        .add_rule(LineCommentRule::new('#'))
        .add_rule(BlockCommentRule::new("#{", "}#"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token {
            token: Token::IntegerLiteral(0),
            location: Span { index: 0, length: 3 },
            lineno: 1,
        } "foo",
        
        token {
            token: Token::Comment,
            location: Span { index: 4, length: 5 },
            lineno: 1,
        } "#bar",
        
        token {
            token: Token::Comment,
            location: Span { index: 20, length: 8 },
            lineno: 3,
        } "#   #{{",
        
        token {
            token: Token::IntegerLiteral(2),
            location: Span { index: 37, length: 3 },
            lineno: 5,
        } "baz",
        
        token {
            token: Token::Comment,
            location: Span { index: 41, length: 65 },
            lineno: 5,
        } "multiline comment block",
    
        token {
            token: Token::IntegerLiteral(1),
            location: Span { index: 107, length: 3 },
            lineno: 10,
        } "bar",
    
        token {
            token: Token::EOF,
            location: Span { index: 120, length: 0 },
            lineno: 12,
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
        .add_rule(ExactRule::new(Token::IntegerLiteral(0), "foo"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(1), "bar"))
        .add_rule(ExactRule::new(Token::IntegerLiteral(2), "baz"))
        .add_rule(LineCommentRule::new('#'))
        .add_rule(BlockCommentRule::new("#{", "}#"))
        .build(source.chars());
    
    assert_token_sequence!(lexer,
    
        token {
            token: Token::IntegerLiteral(0),
            location: Span { index: 0, length: 3 },
            lineno: 1,
        } "foo",
        
        token {
            token: Token::IntegerLiteral(2),
            location: Span { index: 37, length: 3 },
            lineno: 5,
        } "baz",
        
        token {
            token: Token::IntegerLiteral(1),
            location: Span { index: 107, length: 3 },
            lineno: 10,
        } "bar",
        
        token {
            token: Token::EOF,
            location: Span { index: 120, length: 0 },
            lineno: 12,
        } "EOF",
    
    );
}