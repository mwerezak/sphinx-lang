use std::str::Chars;
use std::collections::VecDeque;

use crate::lexer::rules::MatchResult;

// Helper struct to match an exact string

#[derive(Debug, Clone)]
pub struct StrMatcher<'a> {
    target: &'a str,
    state: MatchResult,
    chars: Chars<'a>,
    peek: VecDeque<Option<char>>,
    count: usize, // track how far have we advanced through the target
}

impl<'a> StrMatcher<'a> {
    pub fn new(target: &'a str) -> Self {
        StrMatcher {
            target,
            state: MatchResult::IncompleteMatch,
            chars: target.chars(),
            peek: VecDeque::new(),
            count: 0,
        }
    }
    
    pub fn target(&self) -> &'a str { self.target }
    pub fn last_match(&self) -> MatchResult { self.state }
    pub fn count(&self) -> usize { self.count }
    
    pub fn reset(&mut self) {
        self.state = MatchResult::IncompleteMatch;
        self.chars = self.target.chars();
        self.peek.clear();
        self.count = 0;
    }
    
    pub fn reset_target(&mut self, target: &'a str) {
        self.target = target;
        self.reset();
    }
    
    fn peek_nth(&mut self, n: usize) -> Option<char> {
        while self.peek.len() < n + 1 {
            self.peek.push_back(self.chars.next());
        }
        return self.peek[n];
    }
    
    fn advance(&mut self) -> Option<char> {
        self.count += 1;
        match self.peek.pop_front() {
            Some(o) => o,
            None => self.chars.next()
        }
    }
    
    pub fn peek_match(&mut self, next: char) -> MatchResult {
        // if the match already failed, don't bother looking at any further input
        if !self.state.is_match() {
            return MatchResult::NoMatch;
        }
        
        match self.peek_nth(0) {
            Some(this_ch) if this_ch == next => {
                if self.peek_nth(1).is_none() {
                    MatchResult::CompleteMatch
                } else {
                    MatchResult::IncompleteMatch
                }
            },
            _ => MatchResult::NoMatch,
        }
    }
    
    pub fn update_match(&mut self, next: char) -> MatchResult {
        self.state = self.peek_match(next);
        self.advance();
        return self.state;
    }
    
    pub fn try_match(&mut self, next: char) -> MatchResult {
        let match_result = self.peek_match(next);
        if match_result.is_match() {
            self.state = match_result;
            self.advance();
        }
        return match_result;
    }
    
}