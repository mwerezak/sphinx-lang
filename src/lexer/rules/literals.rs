use std::collections::HashSet;
use crate::lexer::Token;
use super::{MatchResult, LexerRule};

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
                let word_boundary = match prev {
                    Some(ch) if ch == '_' || ch.is_ascii_alphanumeric() => false,
                    _ => true,
                };
                
                match next {
                    '_' | 'a'..='z' | 'A'..='Z' if word_boundary => true,
                    _ => false,
                }
            } else {
                match next {
                    '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'  => true,
                    _ => false,
                }
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