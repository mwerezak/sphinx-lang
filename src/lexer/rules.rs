use std::str::Chars;
use std::iter::Peekable;
use crate::lexer::Token;

// Lexer Rules

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

pub trait LexerRule {
    fn current_state(&self) -> MatchResult;
    
    fn reset(&mut self);
    
    fn feed(&mut self, ch: char) -> MatchResult;
    
    // like feed, but only modifies the LexerRule state if would match
    // return the match state if ch was passed to feed()
    fn try_match(&mut self, ch: char) -> MatchResult;
    
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
    
    fn match_char(&self, ch: char) -> MatchResult {
        if let MatchResult::IncompleteMatch = self.state {
            if ch == self.target {
                return MatchResult::CompleteMatch;
            }
        }
        return MatchResult::NoMatch;
    }
}

impl LexerRule for SingleCharRule {
    fn current_state(&self) -> MatchResult { self.state }
        
    fn feed(&mut self, ch: char) -> MatchResult {
        self.state = self.match_char(ch);
        return self.state;
    }
    
    fn try_match(&mut self, ch: char) -> MatchResult {
        let match_result = self.match_char(ch);
        if !matches!(match_result, MatchResult::NoMatch) {
            self.state = match_result;
        }
        return match_result;
    }
    
    fn reset(&mut self) {
        self.state = MatchResult::IncompleteMatch;
    }
    
    fn get_token(&self) -> Option<Token> {
        match self.state {
            MatchResult::CompleteMatch => Some(self.result.clone()),
            _ => None,
        }
    }
}

// Keyword Rules

#[derive(Debug)]
pub struct ExactRule {
    target: &'static str,
    result: Token,
    state: MatchResult,
    chars: Peekable<Chars<'static>>,
}

impl ExactRule {
    pub fn new(result: Token, target: &'static str) -> Self {
        debug_assert!(!target.is_empty());
        
        ExactRule {
            target, result,
            state: MatchResult::IncompleteMatch,
            chars: target.chars().peekable(),
        }
    }
}

impl LexerRule for ExactRule {
    fn current_state(&self) -> MatchResult { self.state }
    
    fn reset(&mut self) {
        self.state = MatchResult::IncompleteMatch;
        self.chars = self.target.chars().peekable();
    }
    
    fn feed(&mut self, ch: char) -> MatchResult {
        if let MatchResult::NoMatch = self.state {
            return MatchResult::NoMatch;
        }
        
        self.state = match self.chars.next() {
            Some(this_ch) if ch == this_ch => {
                if let None = self.chars.peek() {
                    MatchResult::CompleteMatch
                } else {
                    MatchResult::IncompleteMatch
                }
            },
            _ => MatchResult::NoMatch,
        };
        
        return self.state;
    }
    
    fn try_match(&mut self, ch: char) -> MatchResult {
        if let MatchResult::NoMatch = self.state {
            return MatchResult::NoMatch;
        }
        
        let match_result = match self.chars.peek() {
            Some(&this_ch) if ch == this_ch => {
                
                self.chars.next();
                self.state = if let None = self.chars.peek() {
                    MatchResult::CompleteMatch
                } else {
                    MatchResult::IncompleteMatch
                };
                
                self.state
            },
            _ => MatchResult::NoMatch,
        };
        
        return match_result;
    }
    

    fn get_token(&self) -> Option<Token> {
        match self.state {
            MatchResult::CompleteMatch => Some(self.result.clone()),
            _ => None,
        }
    }
}
