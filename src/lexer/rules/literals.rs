use std::collections::HashSet;
use crate::lexer::Token;
use crate::lexer::rules::{MatchResult, LexerRule, CharClass};
use crate::lexer::rules::strmatcher::StrMatcher;

// Identifiers

#[derive(Debug)]
pub struct IdentifierRule {
    buf: String,
    reserved: HashSet<&'static str>,
}

impl IdentifierRule {
    pub fn new<I>(reserved: I) -> Self 
        where I: IntoIterator<Item=&'static str>
    {
        IdentifierRule {
            buf: String::new(),
            reserved: HashSet::from_iter(reserved),
        }
    }
}

// Identifiers are ( :alphanumeric: | '_' ), first character cannot be a digit
impl LexerRule for IdentifierRule {
    fn reset(&mut self) {
        self.buf.clear();
    }
    
    fn current_state(&self) -> MatchResult { 
        if self.buf.is_empty() {
            MatchResult::IncompleteMatch
        } else if self.reserved.contains(self.buf.as_str()) {
            MatchResult::IncompleteMatch
        } else {
            MatchResult::CompleteMatch
        }
    }
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        let valid = 
            if self.buf.is_empty() {
                let at_word_boundary = match prev {
                    Some(ch) => !ch.is_word_alphanumeric(),
                    None => true,
                };
                at_word_boundary && next.is_word_ascii_alphabetic()
            } else {
                next.is_word_ascii_alphanumeric()
            };
        
        if valid {
            self.buf.push(next);
            self.current_state()
        } else {
            MatchResult::NoMatch
        }
    }
    
    fn get_token(&self) -> Option<Token> {
        if self.current_state().is_complete_match() {
            return Some(Token::Identifier(self.buf.clone()));
        }
        return None;
    }
}

// Plain Integer Literals

pub struct IntegerLiteralRule {
    buf: String,
}

impl IntegerLiteralRule {
    pub fn new() -> Self {
        IntegerLiteralRule { buf: String::new() }
    }
}

impl LexerRule for IntegerLiteralRule {
    fn reset(&mut self) {
        self.buf.clear();
    }
    
    fn current_state(&self) -> MatchResult {
        if self.buf.is_empty() {
            MatchResult::IncompleteMatch
        } else {
            MatchResult::CompleteMatch
        }
    }
    
    fn try_match(&mut self, _prev: Option<char>, next: char) -> MatchResult {
        if next.is_ascii_digit() {
            self.buf.push(next);
            return MatchResult::CompleteMatch;
        }
        return MatchResult::NoMatch;
    }
    
    fn get_token(&self) -> Option<Token> {
        if self.current_state().is_complete_match() {
            let value = i32::from_str_radix(self.buf.as_str(), 10)
                .unwrap_or_else(|err| { 
                    panic!("could not extract i32 from \"{}\": {:?}", self.buf, err); 
                });
            
            return Some(Token::IntegerLiteral(value));
        }
        return None;
    }
    
}

pub struct HexIntegerLiteralRule {
    buf: String,
    prefix: StrMatcher<'static>,
}

impl HexIntegerLiteralRule {
    pub fn new() -> Self {
        HexIntegerLiteralRule {
            buf: String::new(),
            prefix: StrMatcher::case_insensitive("0x"),
        }
    }
}

impl LexerRule for HexIntegerLiteralRule {
    fn reset(&mut self) {
        self.buf.clear();
        self.prefix.reset();
    }
    
    fn current_state(&self) -> MatchResult {
        if self.buf.is_empty() {
            MatchResult::IncompleteMatch
        } else {
            self.prefix.last_match_result()
        }
    }
    
    fn try_match(&mut self, _prev: Option<char>, next: char) -> MatchResult {
        if !self.prefix.last_match_result().is_complete_match() {
            return self.prefix.try_match(next);
        }
        
        if next.is_ascii_hexdigit() {
            self.buf.push(next);
            return MatchResult::CompleteMatch;
        }
        return MatchResult::NoMatch;
    }
    
    fn get_token(&self) -> Option<Token> {
        if self.current_state().is_complete_match() {
            let value = i32::from_str_radix(self.buf.as_str(), 16)
                .unwrap_or_else(|err| { 
                    panic!("could not extract i32 from \"{}\": {:?}", self.buf, err); 
                });
            
            return Some(Token::IntegerLiteral(value));
        }
        return None;
    }
    
}