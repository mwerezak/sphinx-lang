mod token;
mod errors;
mod tests;

pub mod rules;
pub use rules::MatchResult;

pub use token::*;
pub use errors::*;

use std::iter::{Iterator, Peekable};
use crate::language;

use rules::LexerRule;
use rules::comments::{LineCommentRule, BlockCommentRule};


// Token Output

// if one of your source files has more than 4 billion characters (assuming mostly single byte UTF8 that's a ~4GB file)
// or if a token gets longer than 65535 characters, just consider that a lexing error
// In return, we can make the Span struct a fair bit narrower
pub type TokenIndex = u32;  // index of start of token in file
pub type TokenLength = u16;

// include only mere character indexes in the output
// if a lexeme needs to be rendered (e.g. for error messages), 
// the relevant string can be extracted from the source then
#[derive(Clone, Debug)]
pub struct Span {
    pub index: TokenIndex,
    pub length: TokenLength,
}

impl Span {
    pub fn start_index(&self) -> TokenIndex { self.index }
    pub fn end_index(&self) -> TokenIndex { self.index + TokenIndex::from(self.length) }
}

#[derive(Clone, Debug)]
pub struct TokenMeta {
    pub token: Token,
    pub span: Span,
}

// Lexer Builder

#[derive(Clone)]
pub struct LexerOptions {
    skip_comments: bool,
}

pub struct LexerBuilder {
    rules: Vec<Box<dyn LexerRule>>,
    options: LexerOptions,
}

impl LexerBuilder {
    pub fn new() -> Self {
        LexerBuilder {
            rules: Vec::new(),
            options: LexerOptions {
                skip_comments: true,
            }
        }
    }
    
    fn set_options(mut self, options: LexerOptions) -> Self {
        self.options = options;
        return self;
    }
    
    pub fn set_skip_comments(mut self, skip_comments: bool) -> Self {
        self.options.skip_comments = skip_comments;
        return self;
    }
    
    // Note, the order that rules are added determines priority
    
    pub fn add_rule<R>(mut self, rule: R) -> Self
    where R: LexerRule + 'static {
        self.rules.push(Box::new(rule));
        return self;
    }
    
    pub fn insert_rule<R>(mut self, index: usize, rule: R) -> Self
    where R: LexerRule + 'static {
        self.rules.insert(index, Box::new(rule));
        return self;
    }
    
    pub fn extend_rules<I, R>(mut self, rules: I) -> Self
    where I: Iterator<Item=R>, R: LexerRule + 'static {
        for rule in rules {
            self.rules.push(Box::new(rule));
        }
        return self;
    }
    
    // less expensive than build(), but invalidates self
    pub fn build_once<S>(self, source: S) -> Lexer<S> 
    where S: Iterator<Item=char> {
        Lexer::new(source, self.options, self.rules.into_iter())
    }
    
    pub fn build<S>(&self, source: S) -> Lexer<S>
    where S: Iterator<Item=char> {
        Lexer::new(source, self.options.clone(), self.rules.clone().into_iter())
    }
}

// Lexer

fn split_array_pair_mut<T>(pair: &mut [T; 2]) -> (&mut T, &mut T) {
    let (first, rest) = pair.split_first_mut().unwrap();
    let second = &mut rest[0];
    (first, second)
}

fn token_length(start_idx: TokenIndex, end_idx: TokenIndex) -> Result<TokenLength, std::num::TryFromIntError> {
    if end_idx > start_idx { 
        TokenLength::try_from(end_idx - start_idx)
    } else { 
        Ok(0)
    }
}

// to avoid interior self-referentiality inside Lexer (not permitted in safe Rust), 
// instead of passing around references, we pass indices into the rules Vec instead
type RuleID = usize;

pub struct Lexer<S> where S: Iterator<Item=char> {
    source: Peekable<S>,
    options: LexerOptions,
    rules: Vec<Box<dyn LexerRule>>,
    
    current: TokenIndex, // one ahead of current char
    last: Option<char>,
    
    // internal state used by next_token(). 
    // putting these here instead to avoid unnecessary allocations
    active:   [Vec<RuleID>; 2],
    complete: [Vec<RuleID>; 2],
}

// indices for active/complete arrays
const THIS_CYCLE: usize = 0;
const NEXT_CYCLE: usize = 1;


impl<S> Iterator for Lexer<S> where S: Iterator<Item=char> {
    type Item = Result<TokenMeta, LexerError>;
    
    fn next(&mut self) -> Option<Self::Item> { Some(self.next_token()) }
}

type PrevNextChars = (Option<char>, Option<char>);

impl<S> Lexer<S> where S: Iterator<Item=char> {
    
    pub fn new<R>(source: S, options: LexerOptions, rules: R) -> Self
    where R: Iterator<Item=Box<dyn LexerRule>> {
        Lexer {
            options,
            source: source.peekable(),
            rules: rules.collect(),
            
            current: 0,
            last: None,
            active:   [Vec::new(), Vec::new()],
            complete: [Vec::new(), Vec::new()],
        }
    }
    
    
    fn advance(&mut self) -> Result<PrevNextChars, LexerError> {
        self.last = self.peek_next();
        let next = self.source.next();
        
        if next.is_some() {
            if self.current == TokenIndex::MAX {
                return Err(self.error(ErrorKind::SourceTooLong, self.current));
            }
            self.current += 1;
        }
        
        Ok((self.last, next))
    }
    
    // these have to be &mut self because they can mutate the source iterator
    fn peek(&mut self) -> PrevNextChars {
        (self.last, self.peek_next())
    }
    
    fn peek_next(&mut self) -> Option<char> {
        match self.source.peek() {
            Some(&ch) => Some(ch),
            None => None,
        }
    }
    
    pub fn at_eof(&mut self) -> bool {
        self.source.peek().is_none()
    }
    
    fn skip_whitespace(&mut self) -> Result<(), LexerError> {
        let mut next = self.peek_next();
        while next.is_some() && next.unwrap().is_whitespace() {
            self.advance()?;
            next = self.peek_next();
        }
        Ok(())
    }
    
    fn skip_comments(&mut self) -> Result<bool, LexerError> {
        let line_rule = LineCommentRule::new(language::COMMENT_CHAR);
        let block_rule = BlockCommentRule::new(language::NESTED_COMMENT_START, language::NESTED_COMMENT_END);
        
        let mut line = Some(line_rule);
        let mut block = Some(block_rule);
        
        let start_pos = self.current;
        loop {
            let (prev, next) = self.peek();
            let next = match next {
                Some(ch) => ch,
                None => break,
            };
            
            if let Some(rule) = line.as_mut() {
                if !rule.try_match(prev, next).is_match() {
                    line = None;
                }
            }
            
            if let Some(rule) = block.as_mut() {
                if !rule.try_match(prev, next).is_match() {
                    block = None;
                }
            }
            
            if line.is_none() && block.is_none() {
                break;
            }
            
            self.advance()?;
        }
        
        // continue skipping if we are at not at EOF and we advanced
        let continue_ = !self.at_eof() && self.current > start_pos;
        
        Ok(continue_)
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
    
    pub fn next_token(&mut self) -> Result<TokenMeta, LexerError> {
        
        self.skip_whitespace()?;
        
        if self.options.skip_comments {
            while self.skip_comments()? {
                self.skip_whitespace()?;
            }
        }
        
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
        let (mut prev, next) = self.peek();
        let mut next = match next {
            Some(ch) => ch,
            None => {
                return self.token_data(Token::EOF, token_start);
            },
        };
        
        // generate rule ids
        self.active[THIS_CYCLE].extend(0..self.rules.len());
        
        loop {
            
            // need to split the body of this loop into two blocks in order to keep the borrow checker happy...
            
            {
                let (active, next_active) = split_array_pair_mut(&mut self.active);
                let (complete, next_complete) = split_array_pair_mut(&mut self.complete);
                
                // println!("({}) next: {:?}", self.current, next);
                // println!("({}) active: {:?}", self.current, active);
                // println!("({}) complete: {:?}", self.current, complete);
                
                next_active.clear();
                next_complete.clear();
                
                for &rule_id in active.iter() {
                    let rule = &mut self.rules[rule_id];
                    let match_result = rule.try_match(prev, next);
                    
                    if match_result.is_match() {
                        next_active.push(rule_id);
                        
                        if match_result.is_complete_match() {
                            next_complete.push(rule_id);
                        }
                    }
                }
                
                // println!("({}) next_active: {:?}", self.current, next_active);
                // println!("({}) next_complete: {:?}", self.current, next_complete);
                
                
                // Only care about complete rules if next_active is empty ("rule of maximal munch")
                if next_active.is_empty() && !complete.is_empty() {
                    // look at rules that matched the previous char
                    // falling back to the rules which matched completely on the previous char
                    // do not advance the lexer as we will revisit the current char on the next pass
                    
                    // if there is more than one complete rule, the lowest index takes priority!
                    let rule_id = *complete.iter().min().unwrap();
                    let matching_rule = &mut self.rules[rule_id];
                    let token = matching_rule.get_token()
                        .map_err(|err| self.token_error(err, token_start))?;
                    
                    return self.token_data(token, token_start);
                
                }
            }
            
            // commit to accepting this char (and therefore consuming it)
            self.advance()?;
            
            {
                let next_active = &self.active[NEXT_CYCLE];
                
                if next_active.is_empty() {
                    return Err(self.error(ErrorKind::NoMatchingRule, token_start));
                } 
                if next_active.len() == 1 {
                    let rule_id = next_active[0];
                    return self.exhaust_rule(rule_id, token_start);
                }
                
                prev = Some(next);
                next = match self.peek_next() {
                    Some(ch) => ch,
                    None => break,
                };
                
                // swap cycles
                self.active.swap(0, 1);
                self.complete.swap(0, 1);
            }
        }
        
        let next_complete = &self.complete[NEXT_CYCLE];
        if !next_complete.is_empty() {
            
            // if there is more than one complete rule, the lowest index takes priority!
            let rule_id = *next_complete.iter().min().unwrap();
            let matching_rule = &mut self.rules[rule_id];
            let token = matching_rule.get_token()
                .map_err(|err| self.token_error(err, token_start))?;
            
            return self.token_data(token, token_start);
        }
        
        Err(self.error(ErrorKind::UnexpectedEOF, token_start))
    }
    
    fn exhaust_rule(&mut self, rule_id: RuleID, token_start: TokenIndex) -> Result<TokenMeta, LexerError> {
        {
            let rule = &mut self.rules[rule_id];
            debug_assert!(!matches!(rule.current_state(), MatchResult::NoMatch));
        }

        loop {
            let (prev, next) = self.peek();
            let next = match next {
                Some(ch) => ch,
                None => break,
            };
            
            {
                // println!("({}) next: {:?}", self.current, next);
                let rule = &mut self.rules[rule_id];
                match rule.try_match(prev, next) {
                    MatchResult::NoMatch => break,
                    _ => { self.advance()?; },
                }
            }
        }
        
        let rule = &mut self.rules[rule_id];
        if let MatchResult::CompleteMatch = rule.current_state() {
            let token = rule.get_token()
                .map_err(|err| self.token_error(err, token_start))?;
            
            return self.token_data(token, token_start);
        }
        
        if self.at_eof() {
            Err(self.error(ErrorKind::UnexpectedEOF, token_start))
        } else {
            Err(self.error(ErrorKind::NoMatchingRule, token_start))
        }
    }
    
    fn token_data(&self, token: Token, token_start: TokenIndex) -> Result<TokenMeta, LexerError> {
        let length = token_length(token_start, self.current);
        
        let span = Span {
            index: token_start,
            length: length.unwrap_or(0),
        };
        
        if length.is_err() {
            Err(LexerError::new(ErrorKind::MaxTokenLengthExceeded, span))
        } else {
            Ok(TokenMeta { token, span })
        }
    }
    
    fn error(&self, kind: ErrorKind, token_start: TokenIndex) -> LexerError {
        let span = Span {
            index: token_start,
            length: token_length(token_start, self.current).unwrap_or(0),
        };
        LexerError::new(kind, span)
    }
    
    fn token_error(&self, err: Box<dyn std::error::Error>, token_start: TokenIndex) -> LexerError {
        let span = Span {
            index: token_start,
            length: token_length(token_start, self.current).unwrap_or(0),
        };
        LexerError::caused_by(err, ErrorKind::CouldNotReadToken, span)
    }
}
