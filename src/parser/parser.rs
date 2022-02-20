// Recursive descent parser

use std::iter::Peekable;
use crate::lexer::{TokenMeta, Token};
use crate::parser::expr::Expr;
use crate::parser::primary::{Primary, Atom};
use crate::parser::errors::*;

pub struct Parser<T> where T: Iterator<Item=TokenMeta> {
    tokens: Peekable<T>,
}

impl<T> Parser<T> where T: Iterator<Item=TokenMeta> {
    pub fn new(tokens: T) -> Self {
        Parser { tokens: tokens.peekable() }
    }
    
    // should never actually exhaust self.tokens since we will hit EOF first.
    fn advance(&mut self) -> Result<TokenMeta, ParserError> {
        match self.tokens.next() {
            Some(tok) => Ok(tok),
            None => Err(ParserError::new(ErrorKind::RanOutOfTokens)),
        }
    }
    
    fn peek(&mut self) -> Result<&TokenMeta, ParserError> {
        match self.tokens.peek() {
            Some(tok) => Ok(tok),
            None => Err(ParserError::new(ErrorKind::RanOutOfTokens)),
        }
    }
    
    /*** Expression Parsing ***/
    
    /*
        Expression syntax:
    
        expression ::= primary
                     | op-expression
                     | if-expression
                     | assignment-expression 
                     | tuple-constructor
                     | object-constructor
                     | function-def
                     | class-def ;
    */
    fn parse_expr(&mut self) -> Result<Expr, ParserError> { unimplemented!() }
    
    
    /*
        Primary expression syntax:
        
        primary ::= atom ( access | subscript | invocation )* ;
        subscript ::= "[" expression "]" ;
        access ::= "." IDENTIFIER ;
        invocation ::= "(" ... ")" ;  (* WIP *)
    */
    fn parse_primary(&mut self) -> Result<Primary, ParserError> { 
        
        let atom = self.parse_atom();
    
        unimplemented!()
    }
    
    // atom ::= LITERAL | IDENTIFIER | "(" expression ")" ;
    fn parse_atom(&mut self) -> Result<Atom, ParserError> { 
        
        let next = self.advance()?;
        
        match next.token {
            // LITERAL
            Token::Nil => Ok(Atom::Nil),
            Token::True => Ok(Atom::BooleanLiteral(true)),
            Token::False => Ok(Atom::BooleanLiteral(false)),
            Token::IntegerLiteral(value) => Ok(Atom::IntegerLiteral(value)),
            Token::FloatLiteral(value) => Ok(Atom::FloatLiteral(value)),
            // TODO string literals
            
            // IDENTIFIER
            Token::Identifier(name) => Ok(Atom::identifier(name.as_str())),
            
            // "(" expression ")"
            Token::OpenParen => {
                let inner = self.parse_expr()?;
                
                let next = self.advance()?;
                match next.token {
                    Token::CloseParen => Ok(Atom::group(inner)),
                    _ => Err(ParserError::unexpected_token(next, Token::CloseParen))
                }
            },
            
            _ => Err(ParserError::unexpected_token_class(next, TokenClass::Atom))
        }
        
    }
    
    fn parse_binop() { unimplemented!() }
    

}