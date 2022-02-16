use std::str::Chars;
use std::iter::Peekable;
use crate::lexer::Token;

// Lexer Rules

// TODO rename to MatchResult
#[derive(Clone, Copy, Debug)]
pub enum LexerMatch {
    // has not consumed enough characters to produce a valid token, but could if given further correct input
    IncompleteMatch,
    
    // has consumed enough characters to produce a valid token, may still yet accept further correct input
    // should either remain in this state, or drop to the NoMatch state if incorrect input given
    CompleteMatch,
    
    // not a match for the characters that have been given, should remain in this state until reset
    NoMatch,
}

pub trait LexerRule {
    fn current_state(&self) -> LexerMatch;
    
    fn reset(&mut self);
    
    fn feed(&mut self, ch: char) -> LexerMatch;
    
    // like feed, but only modifies the LexerRule state if would match
    // return the match state if ch was passed to feed()
    fn try_match(&mut self, ch: char) -> LexerMatch;
    
    // produce Some(Token) if current state is CompleteMatch, otherwise None
    fn get_token(&self) -> Option<Token>;
}

// Single Character Rules

#[derive(Debug)]
pub struct SingleCharRule {
    target: char,
    state: LexerMatch,
    result: Token,
}

impl SingleCharRule {
    pub fn new(result: Token, target: char) -> Self {
        SingleCharRule {
            target, result,
            state: LexerMatch::IncompleteMatch,
        }
    }
    
    fn match_char(&self, ch: char) -> LexerMatch {
        if ch == self.target {
            LexerMatch::CompleteMatch
        } else {
            LexerMatch::NoMatch
        }
    }
}

impl LexerRule for SingleCharRule {
    fn current_state(&self) -> LexerMatch { self.state }
        
    fn feed(&mut self, ch: char) -> LexerMatch {
        self.state = self.match_char(ch);
        self.state
    }
    
    fn try_match(&mut self, ch: char) -> LexerMatch {
        let match_result = self.match_char(ch);
        if !matches!(match_result, LexerMatch::NoMatch) {
            self.state = match_result;
        }
        return match_result;
    }
    
    fn reset(&mut self) {
        self.state = LexerMatch::IncompleteMatch;
    }
    
    fn get_token(&self) -> Option<Token> {
        match self.state {
            LexerMatch::CompleteMatch => Some(self.result.clone()),
            _ => None,
        }
    }
}

// Keyword Rules

#[derive(Debug)]
pub struct ExactRule {
    target: &'static str,
    result: Token,
    state: LexerMatch,
    chars: Peekable<Chars<'static>>,
}

impl ExactRule {
    pub fn new(result: Token, target: &'static str) -> Self {
        debug_assert!(!target.is_empty());
        
        ExactRule {
            target, result,
            state: LexerMatch::IncompleteMatch,
            chars: target.chars().peekable(),
        }
    }
}

impl LexerRule for ExactRule {
    fn current_state(&self) -> LexerMatch { self.state }
    
    fn reset(&mut self) {
        self.state = LexerMatch::IncompleteMatch;
        self.chars = self.target.chars().peekable();
    }
    
    fn feed(&mut self, ch: char) -> LexerMatch {
        if let LexerMatch::NoMatch = self.state {
            return LexerMatch::NoMatch;
        }
        
        self.state = match self.chars.next() {
            Some(this_ch) if ch == this_ch => {
                if let None = self.chars.peek() {
                    LexerMatch::CompleteMatch
                } else {
                    LexerMatch::IncompleteMatch
                }
            },
            _ => LexerMatch::NoMatch,
        };
        
        return self.state;
    }
    
    fn try_match(&mut self, ch: char) -> LexerMatch {
        if let LexerMatch::NoMatch = self.state {
            return LexerMatch::NoMatch;
        }
        
        let match_result = match self.chars.peek() {
            Some(&this_ch) if ch == this_ch => {
                
                self.chars.next();
                self.state = if let None = self.chars.peek() {
                    LexerMatch::CompleteMatch
                } else {
                    LexerMatch::IncompleteMatch
                };
                
                self.state
            },
            _ => LexerMatch::NoMatch,
        };
        
        return match_result;
    }
    

    fn get_token(&self) -> Option<Token> {
        match self.state {
            LexerMatch::CompleteMatch => Some(self.result.clone()),
            _ => None,
        }
    }
}