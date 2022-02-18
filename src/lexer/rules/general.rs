use crate::lexer::Token;
use super::{MatchResult, LexerRule};
use super::strmatcher::StrMatcher;

// Single Character Rules

#[derive(Debug)]
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
            return MatchResult::CompleteMatch;
        }
        return MatchResult::NoMatch;
    }
}

impl LexerRule for SingleCharRule {
    fn reset(&mut self) {
        self.state = MatchResult::IncompleteMatch;
    }
    
    fn current_state(&self) -> MatchResult { self.state }
    
    fn try_match(&mut self, next: char) -> MatchResult {
        let match_result = self.peek(next);
        if match_result.is_match() {
            self.state = match_result;
        }
        return match_result;
    }
    
    fn get_token(&self) -> Option<Token> {
        if self.state.is_complete_match() {
            return Some(self.result.clone());
        }
        return None;
    }
}

// Keyword Rules

#[derive(Debug)]
pub struct ExactRule {
    result: Token,
    matcher: StrMatcher<'static>,
}

impl ExactRule {
    pub fn new(result: Token, target: &'static str) -> Self {
        debug_assert!(!target.is_empty());
        
        ExactRule {
            result,
            matcher: StrMatcher::new(target),
        }
    }
}

impl LexerRule for ExactRule {
    fn reset(&mut self) {
        self.matcher.reset();
    }
    
    fn current_state(&self) -> MatchResult {
        self.matcher.last_match()
    }
    
    fn try_match(&mut self, next: char) -> MatchResult {
        self.matcher.try_match(next)
    }
    
    fn get_token(&self) -> Option<Token> {
        if self.current_state().is_complete_match() {
            return Some(self.result.clone());
        }
        return None;
    }
}