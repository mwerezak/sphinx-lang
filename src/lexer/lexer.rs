use std::iter::Iterator;
use crate::lexer::Token;


// Lexer Rules

pub enum LexerMatch {
    NoMatch,
    IncompleteMatch,
    CompleteMatch,
}

pub trait LexerRule {
    fn match_state(&self) -> LexerMatch;
    fn feed(&mut self, ch: char) -> LexerMatch;
    fn reset(&mut self);
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
    
    pub fn add_rule<R>(&mut self, rule: R) -> &mut Self
        where R: LexerRule + 'static 
    {
        
        self.rules.push(Box::new(rule));
        return self;
    }
    
    pub fn build<S>(self, source: S) -> Lexer<S> 
        where S: Iterator<Item=char>
    {
        Lexer { 
            source,
            current: 0,
            token_start: 0,
            lineno: 1,
            rules: self.rules,
        }
    }
}

// Lexer

pub struct Lexer<S> where S: Iterator<Item=char> {
    source: S,
    rules: Vec<Box<dyn LexerRule>>,
    
    current: usize,
    token_start: usize,
    lineno: u64,
}

impl<S> Lexer<S> where S: Iterator<Item=char> {
    fn current_span(&self) -> Span {
        Span { index: self.token_start, length: self.current - self.token_start }
    }
    
    pub fn next_token(&mut self) -> Result<TokenOut, LexerError> {
        unimplemented!();
        
        // grab the next char, and feed it to all the rules
        // any rules that no longer match are discarded
        //
        // if exactly one rule left, stop iterating and just fill out that one
        // if nothing left, consider rules that were completed on the last iteration...
        //    if there are none, error (could not parse symbol)
        //    if there are more than one, error (ambiguous symbol)
        //    if there is exactly one, stop iterating and emit a token
        //
        // otherwise...
        //    any rules that match completely are moved to a separate Vec for the next iteration
        //    advance current to the next char
        
    }
    
    fn token_data(&self, token: Token) -> TokenOut {
        TokenOut {
            token,
            location: self.current_span(),
            lineno: self.lineno,
        }
    }
}

// Token Output

// include only mere character indexes in the output
// if a lexeme needs to be rendered, the relevant string can be extracted then
pub struct Span {
    pub index: usize,
    pub length: usize,
}

// uses lifetime of the source text
pub struct TokenOut {
    pub token: Token,
    pub location: Span,
    pub lineno: u64,
}

// Lexer Errors

pub struct LexerError {
    pub message: String,
    pub location: Span,
    pub lineno: u64,
}
