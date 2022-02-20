pub mod strmatcher;
pub mod general;
pub mod literals;
pub mod keywords;
pub mod comments;

pub use general::*;


use std::error::Error;
use crate::lexer::Token;

// Helpers

trait CharClass {
    // alphabetic/alphanumeric + '_'
    fn is_word_alphanumeric(&self) -> bool;
    fn is_word_alphabetic(&self) -> bool;
    fn is_word_ascii_alphanumeric(&self) -> bool;
    fn is_word_ascii_alphabetic(&self) -> bool;
}

impl CharClass for char {
    fn is_word_alphanumeric(&self) -> bool {
        *self == '_' || self.is_alphanumeric()
    }

    fn is_word_alphabetic(&self) -> bool {
        *self == '_' || self.is_alphabetic()
    }

    fn is_word_ascii_alphanumeric(&self) -> bool {
        *self == '_' || self.is_ascii_alphanumeric()
    }

    fn is_word_ascii_alphabetic(&self) -> bool {
        *self == '_' || self.is_ascii_alphabetic()
    }
}

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
type TokenError = Box<dyn Error + 'static>;  // kludge

pub trait LexerRule {
    fn reset(&mut self);
    
    fn current_state(&self) -> MatchResult;
    
    // like feed, but only modifies the LexerRule state if would match
    // return the match state if ch was passed to feed()
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult;
    
    // should always panic if current_state() is not MatchResult::CompleteMatch
    // and produce an error if the Token could not be produced for some other reason
    // e.g. attempting to read an integer literal that overflows
    fn get_token(&self) -> Result<Token, TokenError>;
}
