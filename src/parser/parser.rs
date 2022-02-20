use crate::lexer::{TokenMeta, Token, LexerError};
use crate::parser::expr::Expr;
use crate::parser::primary::{Primary, Atom};
use crate::parser::errors::*;


// Recursive descent parser

// macro_rules! expect_token {
//     () => {};
// }

// fn expect_token(token: TokenMeta, )


// // a "smart pointer" produced when peeking at the next token
// // provides convenience methods to accept() and consome the token
// struct Peek<'a, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
//     parser: &'a mut Parser<T>,
// }

// impl<'a, T> std::ops::Deref for Peek<'a, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
//     type Target = &'a TokenMeta;
//     fn deref(&self) -> &Self::Target;
// }


pub struct Parser<T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    tokens: T,
    next: Option<Result<TokenMeta, ParserError>>
}

impl<T> Parser<T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    pub fn new(tokens: T) -> Self {
        Parser { tokens, next: None, }
    }
    
    fn next_result(&mut self) -> Result<TokenMeta, ParserError> {
        // Running out of tokens is not normal since we should always read an EOF token first
        let result = match self.tokens.next() {
            Some(inner_result) => Ok(inner_result),
            None => Err(ParserError::new(ErrorKind::RanOutOfTokens)),
        };
        
        result?.map_err(|err| ParserError::caused_by(Box::new(err), ErrorKind::LexerError))
    }
    
    fn advance(&mut self) -> Result<TokenMeta, ParserError> {
        self.next.take().unwrap_or_else(|| self.next_result())
    }
    
    fn peek(&mut self) -> Result<&TokenMeta, ParserError> {
        if self.next.is_none() {
            self.next = Some(self.next_result());
        }
        
        // This craziness is needed to finagle a reference in one branch 
        // while advancing the token iterator and taking ownership of the ParserError in the other
        // otherwise the borrow checker will have a heart attack over the immutable borrow in &mut self.
        if self.next.as_ref().unwrap().is_ok() {
            Ok(self.next.as_ref().unwrap().as_ref().unwrap())
        } else {
            Err(self.advance().unwrap_err())
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
    
    fn parse_primary_or_assignment_expr(&mut self) -> Result<Expr, ParserError> {
        
        
        unimplemented!()
    }
    
    /*
        Primary expression syntax:
        
        primary ::= atom ( access | subscript | invocation )* ;
        subscript ::= "[" expression "]" ;
        access ::= "." IDENTIFIER ;
        invocation ::= "(" ... ")" ;  (* WIP *)
    */
    fn parse_primary(&mut self) -> Result<Primary, ParserError> { 
        let mut primary = Primary::new(self.parse_atom()?);
        
        loop {
            match self.peek()?.token {
                
                // access ::= "." IDENTIFIER ;
                Token::OpAccess => {
                    self.advance().unwrap();
                    let next = self.advance()?;
                    if let Token::Identifier(name) = next.token {
                        primary.push_access(name.as_str());
                    } else {
                        return Err(ParserError::unexpected_token(next, Expect::ParseAccessIdentifier));
                    }
                },
                
                // subscript ::= "[" expression "]" ;
                Token::OpenSquare => {
                    self.advance().unwrap();
                    
                    let index_expr = self.parse_expr()?;
                    
                    let next = self.advance()?;
                    if matches!(next.token, Token::CloseSquare) {
                        primary.push_indexing(index_expr);
                    } else {
                        return Err(ParserError::unexpected_token(next, Expect::ParseIndexingCloseSquare));
                    }
                }
                
                // invocation ::= "(" ... ")" ;  (* WIP *)
                Token::OpenParen => {
                    unimplemented!()
                }
                
                _ => break,
            };
        }
    
        Ok(primary)
    }
    
    // atom ::= LITERAL | IDENTIFIER | "(" expression ")" ;
    pub fn parse_atom(&mut self) -> Result<Atom, ParserError> { 
        
        // The nice thing about parsing an atom is that it definitely will consume the next token
        let next = self.advance()?;
        let atom = match next.token {
            // LITERAL
            Token::Nil => Atom::Nil,
            Token::True => Atom::BooleanLiteral(true),
            Token::False => Atom::BooleanLiteral(false),
            Token::IntegerLiteral(value) => Atom::IntegerLiteral(value),
            Token::FloatLiteral(value) => Atom::FloatLiteral(value),
            // TODO string literals
            
            // IDENTIFIER
            Token::Identifier(name) => Atom::identifier(name.as_str()),
            
            // "(" expression ")"
            Token::OpenParen => {
                let inner_expr = self.parse_expr()?;
                
                let next = self.advance()?;
                if !matches!(next.token, Token::CloseParen) {
                    return Err(ParserError::unexpected_token(next, Expect::ParseGroupCloseParen));
                }
                
                Atom::group(inner_expr)
            },
            
            _ => { 
                return Err(ParserError::unexpected_token(next,  Expect::ParseAtom))
            },
        };
        
        Ok(atom)
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