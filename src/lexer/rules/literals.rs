pub mod string;

use std::str::FromStr;
use crate::language;
use crate::lexer::Token;
use crate::lexer::rules::{MatchResult, LexerRule, WordChar, TokenError};
use crate::lexer::rules::strmatcher::StrMatcher;

// Identifiers

fn at_word_start(prev: Option<char>, next: char) -> bool {
    match prev {
        Some(prev) => !prev.is_word_ascii_alphanumeric() && next.is_word_ascii_alphabetic(),
        None => true,
    }
}


#[derive(Debug, Clone)]
pub struct IdentifierRule {
    buf: String,
}

impl IdentifierRule {
    pub fn new() -> Self {
        IdentifierRule { buf: String::new() }
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
        } else {
            MatchResult::CompleteMatch
        }
    }
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        let valid = 
            if self.buf.is_empty() { at_word_start(prev, next) }
            else { next.is_word_ascii_alphanumeric() };
        
        if valid {
            self.buf.push(next);
            
            MatchResult::CompleteMatch
        } else {
            MatchResult::NoMatch
        }
    }
    
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        Ok(Token::Identifier(self.buf.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct BlockLabelRule {
    buf: String,
    prefix: StrMatcher<'static>,
}

impl BlockLabelRule {
    pub fn new(prefix: &'static str) -> Self {
        BlockLabelRule {
            buf: String::new(),
            prefix: StrMatcher::case_sensitive(prefix),
        }
    }
}

impl LexerRule for BlockLabelRule {
    fn reset(&mut self) {
        self.buf.clear();
        self.prefix.reset();
    }
    
    fn current_state(&self) -> MatchResult {
        let match_result = self.prefix.last_match_result();
        
        if !match_result.is_complete_match() {
            match_result
        } else if self.buf.is_empty() {
            MatchResult::IncompleteMatch
        } else {
            MatchResult::CompleteMatch
        }
    }
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        if self.prefix.count() == 0 && !at_word_start(prev, next) {
            return MatchResult::NoMatch;
        }
        
        if !self.prefix.last_match_result().is_complete_match() {
            return self.prefix.try_match(next);
        }
        
        if next.is_word_ascii_alphanumeric() {
            self.buf.push(next);
            
            MatchResult::CompleteMatch
        } else {
            MatchResult::NoMatch
        }
    }
    
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        Ok(Token::Label(self.buf.clone()))
    }
}

// Plain Integer Literals

#[derive(Debug, Clone)]
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
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        if self.buf.is_empty() && matches!(prev, Some(c) if c.is_ascii_digit()) {
            return MatchResult::NoMatch;
        }
        
        if next.is_ascii_digit() {
            self.buf.push(next);
            
            MatchResult::CompleteMatch
        } else {
            MatchResult::NoMatch
        }
    }
    
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        
        let conversion = language::IntType::from_str_radix(self.buf.as_str(), 10);
        match conversion {
            Ok(value) => Ok(Token::IntegerLiteral(value)),
            
            // most likely the value overflowed language::IntType
            Err(err) => Err(Box::new(err)),
        }
    }
    
}

#[derive(Debug, Clone)]
pub struct HexIntegerLiteralRule {
    buf: String,
    prefix: StrMatcher<'static>,
}

impl HexIntegerLiteralRule {
    pub fn new() -> Self {
        HexIntegerLiteralRule {
            buf: String::new(),
            prefix: StrMatcher::ascii_case_insensitive("0x"),
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
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        if self.buf.is_empty() && self.prefix.count() == 0 && matches!(prev, Some(c) if c.is_ascii_digit()) {
            return MatchResult::NoMatch;
        }
        
        if !self.prefix.last_match_result().is_complete_match() {
            return self.prefix.try_match(next);
        }
        
        if next.is_ascii_hexdigit() {
            self.buf.push(next);
            
            MatchResult::CompleteMatch
        } else {
            MatchResult::NoMatch
        }
    }
    
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        
        let conversion = language::IntType::from_str_radix(self.buf.as_str(), 16);
        match conversion {
            Ok(value) => Ok(Token::IntegerLiteral(value)),
            
            // most likely the value overflowed language::IntType
            Err(err) => Err(Box::new(err)),
        }
    }
    
}

// Floating-Point Literals

#[derive(Debug, Clone)]
pub struct FloatLiteralRule {
    buf: String,
    point: bool,
    exp: bool,
    last: Option<char>,
}

impl FloatLiteralRule {
    pub fn new() -> Self {
        FloatLiteralRule { 
            buf: String::new(), 
            point: false,
            exp: false,
            last: None,
        }
    }
}

impl LexerRule for FloatLiteralRule {
    fn reset(&mut self) {
        self.buf.clear();
        self.point = false;
        self.exp = false;
        self.last = None;
    }
    
    fn current_state(&self) -> MatchResult {
        if self.buf.is_empty() {
            MatchResult::IncompleteMatch
        } else if let Some('e' | 'E') = self.last {
            MatchResult::IncompleteMatch
        } else {
            return MatchResult::CompleteMatch;
        }
    }
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        if self.buf.is_empty() {
            if matches!(prev, Some(c) if c.is_ascii_digit()) || matches!(prev, Some('e' | 'E' | '.')) {
                return MatchResult::NoMatch;
            }
        }
        
        if next == '.' {
            if self.point || self.exp {
                return MatchResult::NoMatch;
            }
            
            self.point = true;
            self.buf.push(next);
            self.last = Some(next);
            return MatchResult::CompleteMatch;
        }
        
        if matches!(next, 'e' | 'E') {
            if self.exp {
                return MatchResult::NoMatch;
            }
            
            self.exp = true;
            self.buf.push(next);
            self.last = Some(next);
            return MatchResult::IncompleteMatch;
        }
        
        if next.is_ascii_digit() {
            self.buf.push(next);
            self.last = Some(next);
            MatchResult::CompleteMatch
        } else {
            MatchResult::NoMatch
        }
    }
    
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        
        let conversion = language::FloatType::from_str(self.buf.as_str());
        match conversion {
            Ok(value) => Ok(Token::FloatLiteral(value)),
            
            // most likely the value overflowed language::IntType
            Err(err) => Err(Box::new(err)),
        }
    }
    
}
