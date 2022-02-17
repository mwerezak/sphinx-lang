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
struct StrMatcher<'a> {
    target: &'a str,
    state: MatchResult,
    chars: Chars<'a>,
    peek: VecDeque<Option<char>>,
}

impl<'a> StrMatcher<'a> {
    pub fn new(target: &'a str) -> Self {
        StrMatcher {
            target,
            state: MatchResult::IncompleteMatch,
            chars: target.chars(),
            peek: VecDeque::new(),
        }
    }
    
    pub fn target(&self) -> &'a str { self.target }
    pub fn last_match(&self) -> MatchResult { self.state }
    
    pub fn reset(&mut self) {
        self.state = MatchResult::IncompleteMatch;
        self.chars = self.target.chars();
        self.peek.clear();
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

pub struct BlockCommentRule {
    nestlevel: u32,
    start: StrMatcher<'static>,
    end: StrMatcher<'static>,
}

impl BlockCommentRule {
    pub fn new(start: &'static str, end: &'static str) -> Self {
        BlockCommentRule {
            nestlevel: 0,
            start: StrMatcher::new(start),
            end: StrMatcher::new(end),
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
        if self.end.last_match().is_complete_match() {
            return MatchResult::CompleteMatch;
        }
        return self.start.last_match();
    }
    
    fn try_match(&mut self, next: char) -> MatchResult {

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
    fn get_token(&self) -> Option<Token> {
        if self.current_state().is_complete_match() {
            return Some(Token::Comment);
        }
        return None;
    }
}