use std::str::Chars;
use std::collections::VecDeque;

use crate::lexer::rules::MatchResult;

// Helper struct to match an exact string

#[derive(Debug, Clone)]
pub struct StrMatcher<'a> {
    target: &'a str,
    ignore_ascii_case: bool,
    
    chars: Chars<'a>,
    peek: VecDeque<Option<char>>,
    
    last_result: MatchResult,
    count: usize, // track how far have we advanced through the target
}

impl<'a> StrMatcher<'a> {
    pub fn new(target: &'a str, ignore_ascii_case: bool) -> Self {
        StrMatcher {
            target,
            ignore_ascii_case,
            
            chars: target.chars(),
            peek: VecDeque::new(),
            
            last_result: MatchResult::IncompleteMatch,
            count: 0,
        }
    }
    pub fn case_sensitive(target: &'a str) -> Self { StrMatcher::new(target, false) }
    pub fn case_insensitive(target: &'a str) -> Self { StrMatcher::new(target, true) }
    
    
    pub fn target(&self) -> &'a str { self.target }
    pub fn ignore_ascii_case(&self) -> bool { self.ignore_ascii_case }
    
    pub fn last_match_result(&self) -> MatchResult { self.last_result }
    pub fn count(&self) -> usize { self.count }
    
    pub fn reset(&mut self) {
        self.last_result = MatchResult::IncompleteMatch;
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
        self.peek[n]
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
        if !self.last_result.is_match() {
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
        self.last_result = self.peek_match(next);
        self.advance();
        
        self.last_result
    }
    
    pub fn try_match(&mut self, next: char) -> MatchResult {
        let match_result = self.peek_match(next);
        if match_result.is_match() {
            self.last_result = match_result;
            self.advance();
        }
        match_result
    }
    
}