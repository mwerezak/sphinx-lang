// Recursive descent parser

use std::iter::Peekable;
use crate::lexer::TokenMeta;

pub struct Parser<T> where T: Iterator<Item=TokenMeta> {
    tokens: Peekable<T>,
}

impl<T> Parser<T> where T: Iterator<Item=TokenMeta> {
    pub fn new(tokens: T) -> Self {
        Parser { tokens: tokens.peekable() }
    }
    
    fn advance(&mut self) -> Option<TokenMeta> {
        self.tokens.next()
    }
    
    fn peek(&mut self) -> Option<&TokenMeta> {
        self.tokens.peek()
    }
    
    // Expression Parsing
    
    fn parse_expr() { }
    
    fn parse_primary() { }
    
    fn parse_binop() { }
}