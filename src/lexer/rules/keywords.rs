use crate::lexer::Token;
use super::{MatchResult, LexerRule};
use super::strmatcher::StrMatcher;


#[derive(Debug)]
pub struct KeywordRule {
    result: Token,
    matcher: StrMatcher<'static>,
    first: bool,
}

impl KeywordRule {
    pub fn new(result: Token, target: &'static str) -> Self {
        debug_assert!(!target.is_empty());
        
        KeywordRule {
            result,
            matcher: StrMatcher::new(target),
            first: true,
        }
    }
}

impl LexerRule for KeywordRule {
    fn reset(&mut self) {
        self.matcher.reset();
        self.first = true;
    }
    
    fn current_state(&self) -> MatchResult {
        self.matcher.last_match()
    }
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        if self.first {
            if let Some(ch) = prev {
                if ch == '_' || ch.is_ascii_alphanumeric() {
                    return MatchResult::NoMatch; // must start first char at word boundary
                }
            }
        }
        
        let match_result = self.matcher.try_match(next);
        if match_result.is_match() {
            self.first = false;
        }
        return match_result;
    }
    
    fn get_token(&self) -> Option<Token> {
        if self.current_state().is_complete_match() {
            return Some(self.result.clone());
        }
        return None;
    }
}