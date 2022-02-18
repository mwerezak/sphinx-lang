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
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        let valid = 
            if self.buf.is_empty() {
                let word_boundary = match prev {
                    Some(ch) if ch == '_' || ch.is_ascii_alphanumeric() => false,
                    _ => true,
                };
                
                word_boundary && (next == '_' || next.is_ascii_alphabetic())
                
            } else {
                next == '_' || next.is_ascii_alphanumeric()
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