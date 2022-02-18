use crate::lexer::Token;
use super::{MatchResult, LexerRule};

#[derive(Debug)]
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
    
    fn try_match(&mut self, next: char) -> MatchResult {
        let valid = match next {
            '_' | 'a'..='z' | 'A'..='Z' => true,
            
            // not valid for first character
            '0'..='9' if !self.buf.is_empty() => true,
            
            _ => false,
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