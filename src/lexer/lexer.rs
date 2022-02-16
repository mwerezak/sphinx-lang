use std::iter::{Iterator, Peekable};
use crate::lexer::{Token, LexerRule, MatchResult};
use crate::lexer::{LexerError, LexerErrorType};


// Token Output

// include only mere character indexes in the output
// if a lexeme needs to be rendered, the relevant string can be extracted then
#[derive(Debug)]
pub struct Span {
    pub index: usize,
    pub length: usize,
}

// uses lifetime of the source text
#[derive(Debug)]
pub struct TokenOut {
    pub token: Token,
    pub location: Span,
    pub lineno: u64,
}

// Lexer Builder

pub struct LexerBuilder {
    rules: Vec<Box<dyn LexerRule>>,
}

impl LexerBuilder {
    pub fn new() -> Self {
        LexerBuilder {
            rules: Vec::new(),
        }
    }
    
    pub fn add_rule<R>(mut self, rule: R) -> Self
        where R: LexerRule + 'static 
    {
        self.rules.push(Box::new(rule));
        return self;
    }
    
    pub fn add_rules<I, R>(mut self, rules: I) -> Self
        where I: Iterator<Item=R>, R: LexerRule + 'static
    {
        for rule in rules {
            self.rules.push(Box::new(rule));
        }
        return self;
    }
    
    pub fn build<S>(self, source: S) -> Lexer<S> 
        where S: Iterator<Item=char>
    {
        Lexer { 
            source: source.peekable(),
            rules: self.rules,
            
            current: 0,
            lineno: 1,
        }
    }
}

// Lexer

// TODO operate on bytes, not char?

pub struct Lexer<S> where S: Iterator<Item=char> {
    source: Peekable<S>,
    rules: Vec<Box<dyn LexerRule>>,
    
    current: usize, // one ahead of current char
    lineno: u64,
}


impl<S> Lexer<S> where S: Iterator<Item=char> {
    
    fn advance(&mut self) -> Option<char> {
        let next = self.source.next();
        if let Some(ch) = next {
            self.current += 1;
            if ch == '\n' {
                self.lineno += 1;
            }
        }
        return next;
    }
    
    fn peek(&mut self) -> Option<char> {
        match self.source.peek() {
            Some(&ch) => Some(ch),
            None => None,
        }
    }
    
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                None => break,
                Some(ch) if !ch.is_whitespace() => break,
                _ => { self.advance(); },
            }
        }
    }
    
    fn skip_until_next_line(&mut self) {
        loop {
            match self.advance() {
                None => break,
                Some('\n') => break,
                _ => { }
            }
        }
    }
    
    pub fn next_token(&mut self) -> Result<TokenOut, LexerError> {
        self.skip_whitespace();
        
        //starting a new token
        let token_start = self.current;
        for rule in self.rules.iter_mut() {
            rule.reset();
        }
        
        // check if we are already at EOF
        let mut next = match self.peek() {
            Some(ch) => ch,
            None => {
                return Ok(self.token_data(token_start, Token::EOF));
            },
        };
        
        // grab the next char, and feed it to all the rules
        // any rules that no longer match are discarded
        //
        // if exactly one rule left, stop iterating and just fill out that one
        // if nothing left, consider rules that were completed on the last iteration...
        //    if there are none, error (could not parse symbol)
        //    if there are more than one, error (ambiguous symbol)
        //    if there is exactly one, stop iterating and emit a to;ken
        //
        // otherwise...
        //    any rules that match completely are moved to a separate Vec for the next iteration
        //    advance current to the next char
        
        let mut active: Vec<usize> = (0..self.rules.len()).collect();
        let mut next_active = Vec::<usize>::new();
        
        let mut complete = Vec::<usize>::new();
        let mut next_complete = Vec::<usize>::new();
        
        loop {
            // println!("({}) next: {:?}", self.current, next);
            // println!("({}) active: {:?}", self.current, active);
            
            for rule_idx in active.drain(..) {
                let rule = &mut self.rules[rule_idx];
                match rule.try_match(next) {
                    MatchResult::CompleteMatch => {
                        next_complete.push(rule_idx);
                        next_active.push(rule_idx);
                    },
                    MatchResult::IncompleteMatch => {
                        next_active.push(rule_idx);
                    },
                    MatchResult::NoMatch => { },
                };
            }
            
            // println!("({}) next_active: {:?}", self.current, next_active);
            // println!("({}) complete: {:?}", self.current, complete);
            
            if next_active.is_empty() {
                // look at rules that matched the previous char
                
                if complete.is_empty() {
                    self.advance(); // commit to accepting this char as bad
                    return Err(self.error(token_start, LexerErrorType::NoMatchingRule));
                }
                
                // falling back to the rules which matched completely on the previous char
                // do not advance the lexer as we will revisit the current char on the next pass
                
                if complete.len() > 1 {
                    return Err(self.error(token_start, LexerErrorType::AmbiguousMatch));
                }
                
                let match_idx = complete[0];
                let matching_rule = &mut self.rules[match_idx];
                let token = matching_rule.get_token().unwrap();
                return Ok(self.token_data(token_start, token));
            
            }
            
            self.advance();  // commit to accepting this char as good
            
            if next_active.len() == 1 {
                break;
            }
            
            next = match self.peek() {
                Some(ch) => ch,
                None => break,
            };
            
            let (swap_active, swap_next_active) = (next_active, active);
            active = swap_active;
            next_active = swap_next_active;
            
            let (swap_complete, swap_next_complete) = (next_complete, complete);
            complete = swap_complete;
            next_complete = swap_next_complete;
            next_complete.clear();
        }
        
        // if we get here either there is only one rule left, or we hit EOF
        
        if next_active.len() == 1 {
            let match_idx = next_active[0];
            return self.exhaust_rule(token_start, match_idx);
        }
        if next_complete.len() == 1 {
            let match_idx = next_complete[0];
            let matching_rule = &mut self.rules[match_idx];
            let token = matching_rule.get_token().unwrap();
            return Ok(self.token_data(token_start, token));
        }
        
        return Err(self.error(token_start, LexerErrorType::UnexpectedEOF));
    }
    
    fn exhaust_rule(&mut self, token_start: usize, rule_idx: usize) -> Result<TokenOut, LexerError> {
        {
            let rule = &mut self.rules[rule_idx];
            debug_assert!(!matches!(rule.current_state(), MatchResult::NoMatch));
        }

        loop {
            let next = match self.peek() {
                Some(ch) => ch,
                None => break,
            };
            
            {
                // println!("({}) next: {:?}", self.current, next);
                let rule = &mut self.rules[rule_idx];
                match rule.try_match(next) {
                    MatchResult::NoMatch => break,
                    _ => { self.advance(); },
                }
            }
        }
        
        let rule = &mut self.rules[rule_idx];
        if let MatchResult::CompleteMatch = rule.current_state() {
            let token = rule.get_token().unwrap();
            return Ok(self.token_data(token_start, token));
        }
        
        return Err(self.error(token_start, LexerErrorType::UnexpectedEOF));
    }
    
    fn current_span(&self, start_idx: usize) -> Span {
        let length = if self.current > start_idx { 
            self.current - start_idx 
        } else { 0 };
        
        return Span { index: start_idx, length };
    }
    
    fn token_data(&self, token_start: usize, token: Token) -> TokenOut {
        TokenOut {
            token,
            location: self.current_span(token_start),
            lineno: self.lineno,
        }
    }
    
    fn error(&self, token_start: usize, etype: LexerErrorType) -> LexerError {
        LexerError::new(etype, self.current_span(token_start), self.lineno)
    }
}

