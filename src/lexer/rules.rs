use std::str::Chars;
use std::collections::VecDeque;
use crate::lexer::Token;


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

// Helper struct to match an exact string

#[derive(Debug, Clone)]
struct StrMatcher {
    target: &'static str,
    state: MatchResult,
    chars: Chars<'static>,
    peek: VecDeque<Option<char>>,
}

impl StrMatcher {
    pub fn new(target: &'static str) -> Self {
        StrMatcher {
            target,
            state: MatchResult::IncompleteMatch,
            chars: target.chars(),
            peek: VecDeque::new(),
        }
    }
    
    pub fn target(&self) -> &'static str { self.target }
    pub fn last_match(&self) -> MatchResult { self.state }
    
    pub fn reset(&mut self) {
        self.state = MatchResult::IncompleteMatch;
        self.chars = self.target.chars();
        self.peek.clear();
    }
    
    fn peek_nth(&mut self, n: usize) -> Option<char> {
        while self.peek.len() < n + 1 {
            self.peek.push_back(self.chars.next());
        }
        return self.peek[n];
    }
    
    fn advance(&mut self) -> Option<char> {
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
    matcher: StrMatcher,
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

// Special-Purpose Rules

#[derive(Debug)]
pub struct CommentRule {
    // (start, end)
    // start -> if the comment has started
    // end   -> if the comment has ended
    state: (bool, bool),
    comment: char
}

impl CommentRule {
    pub fn new(comment: char) -> Self {
        CommentRule { comment, state: (false, false) }
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

impl LexerRule for CommentRule {
    fn reset(&mut self) {
        self.state = (false, false);
    }
    
    fn current_state(&self) -> MatchResult {
        self.match_state(self.state)
    }
    
    fn try_match(&mut self, next: char) -> MatchResult {
        let state = self.next_state(self.state, next);
        let match_result = self.match_state(state);
        
        if match_result.is_match() {
            self.state = state;
        }
        
        return match_result;
    }
    
    // produce Some(Token) if current state is CompleteMatch, otherwise None
    fn get_token(&self) -> Option<Token> {
        if self.current_state().is_complete_match() {
            return Some(Token::Comment);
        }
        return None;
    }
}