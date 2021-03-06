use crate::lexer::Token;
use crate::lexer::rules::{MatchResult, LexerRule, TokenError};
use crate::lexer::rules::strmatcher::StrMatcher;


// Single Character Rules

#[derive(Clone)]
pub struct SingleCharRule {
    target: char,
    state: MatchResult,
    result: Token,
}

impl SingleCharRule {
    pub fn new(result: Token, target: char) -> Self {
        SingleCharRule {
            target, result,
            state: MatchResult::IncompleteMatch,
        }
    }
    
    fn peek(&self, next: char) -> MatchResult {
        if self.state.is_incomplete_match() && next == self.target {
            MatchResult::CompleteMatch
        } else {
            MatchResult::NoMatch
        }
    }
}

impl LexerRule for SingleCharRule {
    fn reset(&mut self) {
        self.state = MatchResult::IncompleteMatch;
    }
    
    fn current_state(&self) -> MatchResult { self.state }
    
    fn try_match(&mut self, _prev: Option<char>, next: char) -> MatchResult {
        let match_result = self.peek(next);
        if match_result.is_match() {
            self.state = match_result;
        }
        match_result
    }
    
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        Ok(self.result.clone())
    }
}

// Multi Character Rules

#[derive(Clone)]
pub struct MultiCharRule {
    result: Token,
    matcher: StrMatcher<'static>,
}

impl MultiCharRule {
    pub fn new(result: Token, target: &'static str) -> Self {
        MultiCharRule {
            result,
            matcher: StrMatcher::case_sensitive(target),
        }
    }
}

impl LexerRule for MultiCharRule {
    fn reset(&mut self) {
        self.matcher.reset();
    }
    
    fn current_state(&self) -> MatchResult {
        self.matcher.last_match_result()
    }
    
    fn try_match(&mut self, _prev: Option<char>, next: char) -> MatchResult {
        self.matcher.try_match(next)
    }
    
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        Ok(self.result.clone())
    }
}