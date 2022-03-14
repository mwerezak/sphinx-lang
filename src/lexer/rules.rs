pub mod strmatcher;
pub mod general;
pub mod literals;
pub mod keywords;
pub mod comments;

pub use general::*;


use std::error::Error;
use crate::lexer::Token;

// Helpers

// "word" characters are the alphabetic/alphanumeric chars + '_' (underscore)
trait WordChar {
    fn is_word_alphanumeric(&self) -> bool;
    fn is_word_alphabetic(&self) -> bool;
    fn is_word_ascii_alphanumeric(&self) -> bool;
    fn is_word_ascii_alphabetic(&self) -> bool;
}

impl WordChar for char {
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

#[derive(Clone, Copy)]
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
    
    pub fn is_complete_match(&self) -> bool { matches!(self, MatchResult::CompleteMatch) }
    
    pub fn is_incomplete_match(&self) -> bool { matches!(self, MatchResult::IncompleteMatch) }
}

// Lexer Rules
type TokenError = Box<dyn Error + 'static>;

pub trait LexerRule: __LexerRule_Clone {
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


// In order to use LexerBuilder as a Lexer factory, we need to be able to clone LexerRules.
// Since each LexerRule is actually a state machine, and in fact likely constitutes most of the state
// used by a Lexer, each new Lexer must own its own LexerRules.

// Unfortunately, trait objects are not clonable because any trait with Fn() -> Self is not object safe.
// However, we can work around this to make Box<dyn LexerRule> cloneable...

#[allow(non_camel_case_types)]
pub trait __LexerRule_Clone {
    fn __clone_box(&self) -> Box<dyn LexerRule>;
}

impl<T> __LexerRule_Clone for T where T: 'static + LexerRule + Clone {
    fn __clone_box(&self) -> Box<dyn LexerRule> { Box::new(self.clone()) }
}

impl Clone for Box<dyn LexerRule> {
    fn clone(&self) -> Box<dyn LexerRule> { self.__clone_box() }
}
