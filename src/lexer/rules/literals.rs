use core::str::FromStr;
use crate::language;
use crate::lexer::Token;
use crate::lexer::rules::{MatchResult, LexerRule, WordChar, TokenError};
use crate::lexer::rules::strmatcher::StrMatcher;

pub mod string;

// Identifiers

#[derive(Clone)]
pub struct IdentifierRule {
    buf: String,
}

impl Default for IdentifierRule {
    fn default() -> Self { Self::new() }
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
        
        let valid;
        if self.buf.is_empty() {
            let at_word_start = prev.map(|c| !c.is_word_ascii_alphanumeric()).unwrap_or(true);
            valid = at_word_start && next.is_word_ascii_alphabetic();
        } else {
            valid = next.is_word_ascii_alphanumeric();
        }
        
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

#[derive(Clone)]
pub struct LabelRule {
    buf: String,
    prefix: StrMatcher<'static>,
}

impl LabelRule {
    pub fn new(prefix: &'static str) -> Self {
        LabelRule {
            buf: String::new(),
            prefix: StrMatcher::case_sensitive(prefix),
        }
    }
}

impl LexerRule for LabelRule {
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
        // don't match if the last char was word alphanumeric
        let at_word_start = prev.map(|c| !c.is_word_ascii_alphanumeric()).unwrap_or(true);
        
        if self.buf.is_empty() && self.prefix.count() == 0 && !at_word_start {
            return MatchResult::NoMatch;
        }
        
        if !self.prefix.last_match_result().is_complete_match() {
            let match_result = self.prefix.try_match(next);
            if match_result.is_complete_match() {
                return MatchResult::IncompleteMatch;
            }
            return match_result;
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

#[derive(Clone)]
pub struct IntegerLiteralRule {
    buf: String,
}

impl Default for IntegerLiteralRule {
    fn default() -> Self { Self::new() }
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

#[derive(Clone)]
pub struct PrefixedIntegerLiteralRule {
    buf: String,
    prefix: StrMatcher<'static>,
    radix: u32,
}

impl PrefixedIntegerLiteralRule {
    pub fn new(prefix: &'static str, radix: u32) -> Self {
        PrefixedIntegerLiteralRule {
            buf: String::new(),
            prefix: StrMatcher::ascii_case_insensitive(prefix),
            radix,
        }
    }
}

impl LexerRule for PrefixedIntegerLiteralRule {
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
        
        let conversion = language::IntType::from_str_radix(self.buf.as_str(), self.radix);
        match conversion {
            Ok(value) => Ok(Token::IntegerLiteral(value)),
            
            // most likely the value overflowed language::IntType
            Err(err) => Err(Box::new(err)),
        }
    }
    
}

// Floating-Point Literals

#[derive(Clone)]
pub struct FloatLiteralRule {
    buf: String,
    point: bool,
    exp: bool,
    last: Option<char>,
}

impl Default for FloatLiteralRule {
    fn default() -> Self { Self::new() }
}

impl FloatLiteralRule {
    pub fn new() -> Self {
        Self { 
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
        if self.buf.is_empty() || matches!(self.last, Some('e' | 'E')) {
            MatchResult::IncompleteMatch
        } else {
            MatchResult::CompleteMatch
        }
    }
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        if self.buf.is_empty() && (matches!(prev, Some(c) if c.is_ascii_digit()) || matches!(prev, Some('e' | 'E' | '.'))) {
            return MatchResult::NoMatch;
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
