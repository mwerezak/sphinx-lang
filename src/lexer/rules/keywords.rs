use crate::lexer::Token;
use crate::lexer::rules::{MatchResult, LexerRule, WordChar, TokenError};
use crate::lexer::rules::strmatcher::StrMatcher;


// Similar to MultiCharRule but also ensures that the token starts at a word boundary

#[derive(Debug, Clone)]
pub struct KeywordRule {
    result: Token,
    matcher: StrMatcher<'static>,
}

impl KeywordRule {
    pub fn new(result: Token, target: &'static str) -> Self {
        debug_assert!(!target.is_empty());
        
        KeywordRule {
            result,
            matcher: StrMatcher::case_sensitive(target),
        }
    }
}

impl LexerRule for KeywordRule {
    fn reset(&mut self) {
        self.matcher.reset();
    }
    
    fn current_state(&self) -> MatchResult {
        self.matcher.last_match_result()
    }
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        if self.matcher.count() == 0 {
            let at_word_boundary = match prev {
                Some(ch) => !ch.is_word_alphanumeric(),
                None => true,
            };
            if !at_word_boundary {
                return MatchResult::NoMatch; // must start first char at word boundary
            }
        }
        
        self.matcher.try_match(next)
    }
    
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        Ok(self.result.clone())
    }
}