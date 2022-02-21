use crate::lexer::Token;
use crate::lexer::rules::{MatchResult, LexerRule, TokenError};
use crate::lexer::rules::strmatcher::StrMatcher;

// Special-Purpose Rules

#[derive(Debug)]
pub struct LineCommentRule {
    // (start, end)
    // start -> if the comment has started
    // end   -> if the comment has ended
    state: (bool, bool),
    comment: char
}

impl LineCommentRule {
    pub fn new(comment: char) -> Self {
        LineCommentRule { comment, state: (false, false) }
    }
    
    fn match_state(&self, state: (bool, bool)) -> MatchResult {
        match state {
            (_, false) => MatchResult::CompleteMatch,
            (true, _)  => MatchResult::CompleteMatch,
            (false, true) => MatchResult::NoMatch,
        }
    }
    
    fn next_state(&self, state: (bool, bool), next: char) -> (bool, bool) {
        let (start, end) = state;
        
        // looking for initial comment char
        if !start {
            if next != self.comment {
                return (false, true);
            }
            return (true, false);
        }
        
        // looking for end of comment
        if start && !end {
            if next == '\n' {
                return (true, true);
            }
            return (true, false);
        }
        
        // complete comment - anything else will not match
        return (false, true);
    }
}

impl LexerRule for LineCommentRule {
    fn reset(&mut self) {
        self.state = (false, false);
    }
    
    fn current_state(&self) -> MatchResult {
        self.match_state(self.state)
    }
    
    fn try_match(&mut self, _prev: Option<char>, next: char) -> MatchResult {
        let state = self.next_state(self.state, next);
        let match_result = self.match_state(state);
        
        if match_result.is_match() {
            self.state = state;
        }
        
        match_result
    }
    
    // produce Some(Token) if current state is CompleteMatch, otherwise None
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        Ok(Token::Comment)
    }
}

pub struct BlockCommentRule {
    nestlevel: u32,
    start: StrMatcher<'static>,
    end: StrMatcher<'static>,
}

impl BlockCommentRule {
    pub fn new(start: &'static str, end: &'static str) -> Self {
        BlockCommentRule {
            nestlevel: 0,
            start: StrMatcher::case_sensitive(start),
            end: StrMatcher::case_sensitive(end),
        }
    }
}

impl LexerRule for BlockCommentRule {
    fn reset(&mut self) {
        self.nestlevel = 0;
        self.start.reset();
        self.end.reset();
    }
    
    fn current_state(&self) -> MatchResult {
        if self.nestlevel > 0 {
            return MatchResult::IncompleteMatch;
        }
        
        if self.end.last_match_result().is_complete_match() {
            return MatchResult::CompleteMatch;
        }
        
        self.start.last_match_result()
    }
    
    fn try_match(&mut self, _prev: Option<char>, next: char) -> MatchResult {

        let start_result = self.start.try_match(next);
        if start_result.is_complete_match() {
            self.nestlevel += 1;
            self.start.reset();
            return MatchResult::IncompleteMatch;
        }
        
        if self.nestlevel > 0 {
            let end_result = self.end.try_match(next);
            if end_result.is_complete_match() {
                self.nestlevel -= 1;
                
                if self.nestlevel > 0 {
                    self.end.reset();
                } else {
                    return MatchResult::CompleteMatch;
                }
            }
            
            if !start_result.is_match() {
                self.start.reset();
            }
            if !end_result.is_match() {
                self.end.reset();
            }
            
            return MatchResult::IncompleteMatch;
        }
        
        return start_result;
    }
    
    // produce Some(Token) if current state is CompleteMatch, otherwise None
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        Ok(Token::Comment)
    }
}