use crate::lexer::Token;

pub mod strmatcher;
pub mod general;
pub mod literals;
pub mod comments;

pub use general::*;



// Match Result

#[derive(Clone, Copy, Debug)]
pub enum MatchResult {
    // has not consumed enough characters to produce a valid token, but could if given further correct input
    IncompleteMatch,
    
    // has consumed enough characters to produce a valid token, may still yet accept further correct input
    // should either remain in this state, or drop to the NoMatch state if incorrect input given
    CompleteMatch,
    
    // not a match for the characters that have been given, should remain in this state until reset
    NoMatch,
}

impl MatchResult {
    pub fn is_match(&self) -> bool {
        match self {
            MatchResult::IncompleteMatch | MatchResult::CompleteMatch => true,
            MatchResult::NoMatch => false,
        }
    }
    
    pub fn is_complete_match(&self) -> bool {
        if let MatchResult::CompleteMatch = self { true } else { false }
    }
    
    pub fn is_incomplete_match(&self) -> bool {
        if let MatchResult::IncompleteMatch = self { true } else { false }
    }
}

// Lexer Rules

pub trait LexerRule {
    fn reset(&mut self);
    
    fn current_state(&self) -> MatchResult;
    
    // like feed, but only modifies the LexerRule state if would match
    // return the match state if ch was passed to feed()
    fn try_match(&mut self, next: char) -> MatchResult;
    
    // produce Some(Token) if current state is CompleteMatch, otherwise None
    fn get_token(&self) -> Option<Token>;
}
