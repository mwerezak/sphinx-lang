mod token;
mod errors;
mod tests;

pub mod rules;
pub use rules::MatchResult;

pub use token::*;
pub use errors::*;

use std::io;
use std::iter::{Iterator, Peekable};
use crate::language;

use rules::LexerRule;
use rules::comments::{LineCommentRule, BlockCommentRule};


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
        self
    }
    
    pub fn set_skip_comments(mut self, skip_comments: bool) -> Self {
        self.options.skip_comments = skip_comments;
        self
    }
    
    // Note, the order that rules are added determines priority
    
    pub fn add_rule<R>(mut self, rule: R) -> Self
    where R: LexerRule + 'static {
        self.rules.push(Box::new(rule));
        self
    }
    
    pub fn insert_rule<R>(mut self, index: usize, rule: impl LexerRule + 'static) -> Self {
        self.rules.insert(index, Box::new(rule));
        self
    }
    
    pub fn extend_rules(mut self, rules: impl Iterator<Item=impl LexerRule + 'static>) -> Self {
        for rule in rules {
            self.rules.push(Box::new(rule));
        }
        self
    }
    
    // less expensive than build(), but invalidates self
    pub fn build_once<S>(self, source: S) -> Lexer<S> where S: Iterator<Item=io::Result<char>> {
        
        Lexer::new(source, self.options, self.rules.into_iter())
        
    }
    
    pub fn build<S>(&self, source: S) -> Lexer<S> where S: Iterator<Item=io::Result<char>> {
        
        Lexer::new(source, self.options.clone(), self.rules.clone().into_iter())
        
    }
}

// Lexer

fn split_array_pair_mut<T>(pair: &mut [T; 2]) -> (&mut T, &mut T) {
    let (first, rest) = pair.split_first_mut().unwrap();
    let second = &mut rest[0];
    (first, second)
}

fn token_length(start_idx: &TokenIndex, end_idx: &TokenIndex) -> Result<TokenLength, std::num::TryFromIntError> {
    if end_idx > start_idx { 
        TokenLength::try_from(end_idx - start_idx)
    } else { 
        Ok(0)
    }
}

// to avoid interior self-referentiality inside Lexer (not permitted in safe Rust), 
// instead of passing around references, we pass indices into the rules Vec instead
type RuleID = usize;

pub struct Lexer<S> where S: Iterator<Item=io::Result<char>> {
    source: Peekable<S>,
    options: LexerOptions,
    rules: Vec<Box<dyn LexerRule>>,
    
    current: TokenIndex, // one ahead of current char
    last: Option<char>,
    newline: bool,
    
    // internal state used by next_token(). 
    // putting these here instead to avoid unnecessary allocations
    active:   [Vec<RuleID>; 2],
    complete: [Vec<RuleID>; 2],
}

// indices for active/complete arrays
const THIS_CYCLE: usize = 0;
const NEXT_CYCLE: usize = 1;


impl<S> Iterator for Lexer<S> where S: Iterator<Item=io::Result<char>> {
    type Item = Result<TokenMeta, LexerError>;
    
    fn next(&mut self) -> Option<Self::Item> { Some(self.next_token()) }
}

type PrevNextChars = (Option<char>, Option<char>);

impl<S> Lexer<S> where S: Iterator<Item=io::Result<char>> {
    
    pub fn new(source: S, options: LexerOptions, rules: impl Iterator<Item=Box<dyn LexerRule>>) -> Self {
        Lexer {
            options,
            source: source.peekable(),
            rules: rules.collect(),
            
            current: 0,
            last: None,
            newline: true,
            active:   [Vec::new(), Vec::new()],
            complete: [Vec::new(), Vec::new()],
        }
    }
    
    // grab the next character from source, transposing any io::Error and mapping it to LexerError
    fn get_next(&mut self) -> Result<Option<char>, LexerError> {
        match self.source.next() {
            None => Ok(None),
            Some(result) => match result {
                Ok(c) => Ok(Some(c)),
                Err(error) => Err(self.inner_error(Box::new(error), ErrorKind::IOError, self.current)),
            },
        }
    }
    
    fn peek_next(&mut self) -> Result<Option<char>, LexerError> {
        let result = match self.source.peek() {
            None => Ok(None),
            Some(Ok(c)) => Ok(Some(*c)),
            
            // if there is an IO error we cannot process it yet, since peek() merely borrows the error
            Some(Err(..)) => Err(()),
        };
        
        result.map_err(|_| {
            let ioerror = self.source.next().unwrap().unwrap_err();
            self.inner_error(Box::new(ioerror), ErrorKind::IOError, self.current)
        })
    }
    
    fn advance(&mut self) -> Result<PrevNextChars, LexerError> {
        self.last = self.peek_next()?;
        let next = self.get_next()?;
        
        if next.is_some() {
            if self.current == TokenIndex::MAX {
                return Err(self.error(ErrorKind::SourceTooLong, self.current));
            }
            self.current += 1;
        }
        
        Ok((self.last, next))
    }
    
    // these have to be &mut self because they can mutate the source iterator
    fn peek(&mut self) -> Result<PrevNextChars, LexerError> {
        Ok((self.last, self.peek_next()?))
    }
    
    pub fn at_eof(&mut self) -> bool {
        self.source.peek().is_none()
    }
    
    fn skip_whitespace(&mut self) -> Result<(), LexerError> {
        let mut next = self.peek_next()?;
        while next.is_some() && next.unwrap().is_whitespace() {
            // consume whitespace and update self.newline
            if let (_, Some('\n')) = self.advance()? {
                self.newline = true;
            }
            next = self.peek_next()?;
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
            let (prev, next) = self.peek()?;
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
            
            // consume comment char and update self.newline
            if let (_, Some('\n')) = self.advance()? {
                self.newline = true;
            }
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
        
        let result = self.scan_token();
        self.newline = matches!(self.last, Some('\n'));
        
        result
    }
    
    fn scan_token(&mut self) -> Result<TokenMeta, LexerError> {
        
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
        let (mut prev, next) = self.peek()?;
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
                        .map_err(|err| self.inner_error(err, ErrorKind::CouldNotReadToken, token_start))?;
                    
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
                next = match self.peek_next()? {
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
                .map_err(|err| self.inner_error(err, ErrorKind::CouldNotReadToken, token_start))?;
            
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
            let (prev, next) = self.peek()?;
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
        if matches!(rule.current_state(), MatchResult::CompleteMatch) {
            let token = rule.get_token()
                .map_err(|err| self.inner_error(err, ErrorKind::CouldNotReadToken, token_start))?;
            
            return self.token_data(token, token_start);
        }
        
        if self.at_eof() {
            Err(self.error(ErrorKind::UnexpectedEOF, token_start))
        } else {
            Err(self.error(ErrorKind::NoMatchingRule, token_start))
        }
    }
    
    fn token_data(&self, token: Token, token_start: TokenIndex) -> Result<TokenMeta, LexerError> {
        let length = token_length(&token_start, &self.current);
        
        let span = Span {
            index: token_start,
            length: length.unwrap_or(0),
        };
        
        if length.is_err() {
            Err(LexerError::new(ErrorKind::MaxTokenLengthExceeded, span))
        } else {
            Ok(TokenMeta { token, span, newline: self.newline })
        }
    }
    
    fn error(&self, kind: ErrorKind, token_start: TokenIndex) -> LexerError {
        let span = Span {
            index: token_start,
            length: token_length(&token_start, &self.current).unwrap_or(0),
        };
        LexerError::new(kind, span)
    }
    
    fn inner_error(&self, err: Box<dyn std::error::Error>, kind: ErrorKind, token_start: TokenIndex) -> LexerError {
        let span = Span {
            index: token_start,
            length: token_length(&token_start, &self.current).unwrap_or(0),
        };
        LexerError::new(kind, span).caused_by(err)
    }
}
