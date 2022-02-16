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
            
            active:   [Vec::new(), Vec::new()],
            complete: [Vec::new(), Vec::new()],
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
    
    // internal state used by next_token(). 
    // putting these here instead to avoid unnecessary allocations
    active:   [Vec<usize>; 2],
    complete: [Vec<usize>; 2],
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
        let mut next = self.peek();
        while next.is_some() && next.unwrap().is_whitespace() {
            self.advance();
            next = self.peek();
        }
    }
    
    fn skip_until_next_line(&mut self) {
        let mut next = self.advance();
        while next.is_some() && next.unwrap() != '\n' {
            next = self.advance();
        }
    }
    
    fn reset_rules(&mut self) {
        for rule in self.rules.iter_mut() {
            rule.reset();
        }
        
        for idx in 0..2 {
            self.active[idx].clear();
            self.complete[idx].clear();
        }
    }
    
    pub fn next_token(&mut self) -> Result<TokenOut, LexerError> {
        
        self.skip_whitespace();
        
        //starting a new token
        let token_start = self.current;
        self.reset_rules();
        
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
        
        // check if we are already at EOF
        let mut next = match self.peek() {
            Some(ch) => ch,
            None => {
                return Ok(self.token_data(token_start, Token::EOF));
            },
        };
        
        self.active[0].extend(0..self.rules.len());
        
        loop {
            
            // need to split the body of this loop into two blocks in order to keep the borrow checker happy...
            
            {
                let (active, next_active) = split_array_pair_mut(&mut self.active);
                let (complete, next_complete) = split_array_pair_mut(&mut self.complete);
                
                // println!("({}) next: {:?}", self.current, next);
                // println!("({}) active: {:?}", self.current, active);
                
                next_active.clear();
                next_complete.clear();
                
                for &rule_idx in active.iter() {
                    let rule = &mut self.rules[rule_idx];
                    let match_result = rule.try_match(next);
                    
                    if match_result.is_match() {
                        next_active.push(rule_idx);
                        
                        if match_result.is_complete_match() {
                            next_complete.push(rule_idx);
                        }
                    }
                }
                
                
                // println!("({}) next_active: {:?}", self.current, next_active);
                // println!("({}) complete: {:?}", self.current, complete);
                
                if next_active.is_empty() && !complete.is_empty() {
                    // look at rules that matched the previous char
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
            }
            
            // commit to accepting this char (and therefore consuming it)
            self.advance();
            
            {
                let next_active = &self.active[1];
                
                if next_active.is_empty() {
                    return Err(self.error(token_start, LexerErrorType::NoMatchingRule));
                } 
                if next_active.len() == 1 {
                    let match_idx = next_active[0];
                    return self.exhaust_rule(token_start, match_idx);
                }
                
                next = match self.peek() {
                    Some(ch) => ch,
                    None => break,
                };
                
                // swap cycles
                self.active.swap(0, 1);
                self.complete.swap(0, 1);
            }
        }
        
        let next_complete = &self.complete[1];
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


// Helpers

fn split_array_pair_mut<T>(pair: &mut [T; 2]) -> (&mut T, &mut T) {
    let (first, rest) = pair.split_first_mut().unwrap();
    let second = &mut rest[0];
    (first, second)
}
