// Recursive descent parser

use std::iter::Peekable;
use crate::lexer::{TokenMeta, Token, LexerError};
use crate::parser::expr::Expr;
use crate::parser::primary::{Primary, Atom};
use crate::parser::errors::*;

pub struct Parser<T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    tokens: Peekable<T>,
}

impl<T> Parser<T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    pub fn new(tokens: T) -> Self {
        Parser { tokens: tokens.peekable() }
    }
    
    // should never actually exhaust self.tokens since we will hit EOF first.
    fn advance(&mut self) -> Result<TokenMeta, ParserError> {
        let result = match self.tokens.next() {
            Some(inner_result) => Ok(inner_result),
            None => Err(ParserError::new(ErrorKind::RanOutOfTokens)),
        };
        
        result?.map_err(|err| ParserError::caused_by(Box::new(err), ErrorKind::LexerError))
    }
    
    // fn peek(&mut self) -> Result<&TokenMeta, ParserError> {
    //     {
    //         let result = match self.tokens.peek() {
    //             Some(result) => result,  // this is a reference to a result
    //             None => {
    //                 return Err(ParserError::new(ErrorKind::RanOutOfTokens));
    //             }
    //         };
            

    //         if result.is_ok() {
    //             return Ok(result.as_ref().unwrap());
    //         }
    //     }
        
    //     // in case of an error, we will need to take ownership, so we have to advance
    //     // discard result (which is a ref anyways) and return the actual thing
    //     return Err(self.advance().unwrap_err());
    // }
    
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
    pub fn parse_atom(&mut self) -> Result<Atom, ParserError> { 
        
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
    
    /*
        Binary operator syntax:
        
        operand[1] ::= unary ;
        operand[8] ::= comparison ;
        operand[N] ::= operand[N-1] ( OPERATOR[N] operand[N-1] )* ;
    */
    fn parse_binop(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }
    
    
    
    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        unimplemented!()
    }
    
    
    // Discards tokens until we reach a statement boundary
    fn synchronize_stmt(&mut self) {
        unimplemented!()
    }

}