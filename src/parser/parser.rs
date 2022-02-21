use crate::lexer::{TokenMeta, Token, LexerError};
use crate::parser::expr::Expr;
use crate::parser::primary::{Primary, Atom};
use crate::parser::operator::BinaryOp;
use crate::parser::structs::{ObjectConstructor, TupleConstructor};
use crate::parser::errors::{ParserError, ErrorKind, ErrorContext};
use crate::parser::debug::DebugMeta;



// Recursive descent parser

pub struct Parser<'n, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    name: &'n str,
    tokens: T,
    next: Option<Result<TokenMeta, ParserError>>,
}

impl<'n, T> Parser<'n, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    pub fn new(name: &'n str, tokens: T) -> Self {
        Parser { 
            name,
            tokens, 
            next: None, 
        }
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
            Ok(self.next.as_ref().unwrap().as_ref().unwrap()) // yes, the repetition is required
        } else {
            Err(self.advance().unwrap_err())
        }
    }
    
    pub fn next_expr(&mut self) -> Result<Expr, (ParserError, DebugMeta)> { 
        let ctx = ErrorContext::new();
        
        match self.parse_expr(&mut ctx) {
            Ok(expr) => {
                // grab debugging info from the current context and attach it to the result
            },
            Err(err) => {
                // grab debugging info from the current context 
                // and return it with the error after synchronizing
                
                return Err((err, ctx.frame().dbg_info(self.name))); // TODO synchronize
            },
        }
        
        
        unimplemented!()
    }
    
    /*** Expression Parsing ***/
    
    /*
        Expression syntax:
    
        expression ::= primary
                     | op-expression
                     | assignment-expression 
                     | if-expression
                     | function-def
                     | class-def
                     | tuple-constructor ;
                     
        Note: because determining if the expression is an assignment-expression or an object-constructor
        requires consuming a primary expression, check for unary operators first
    */
    fn parse_expr(&mut self, ctx: &mut ErrorContext) -> Result<Expr, ParserError> { 
        ctx.push();
        
        let next = self.peek()?;
        ctx.set_start(&next);
        
        let expr = match next.token {
            Token::Class => unimplemented!(),
            Token::Fun => unimplemented!(),
            Token::If => unimplemented!(),
            Token::OpenBrace => unimplemented!(), // anonymous object constructor
            Token::Var => unimplemented!(),
            _ => {
                // at this point, we need to parse a primary expression to see if this is an object constructor
                
                unimplemented!()
            }
        };
        
        unimplemented!() 
    }
    
    
    /*
        Assignment Expression syntax:
        
        assignment-expression ::= assignment-target ( "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" ) expression ;
        assignment-target ::= ( "var" )? lvalue ; 
    */
    
    fn parse_assignment_expr(&mut self, primary: Primary, ctx: &mut ErrorContext) -> Result<Expr, ParserError> {
        
        let assignment = self.peek_next_assignment_op()?;

        if let Some(op) = assignment {
            // if this is an assignment then primary needs to be an lvalue
            
            // consume the "=" token
            ctx.set_end(&self.advance().unwrap()); 
            
            if !primary.is_lvalue() {
                return Err(ParserError::new(ErrorKind::InvalidAssignmentLHS))
            }
            
            let rhs_expr = self.parse_expr(ctx)?;
            
            Ok(Expr::assignment(primary, op, rhs_expr, false))
            
        } else {
            
            Ok(Expr::Primary(primary))
        }
    }
    
    // should only be called if the next token is "var"
    fn parse_var_decl_assignment_expr(&mut self, ctx: &mut ErrorContext) -> Result<Expr, ParserError> {
        ctx.push();
        
        let next = self.advance().unwrap(); // consume the "var"
        ctx.set_start(&next);
        
        debug_assert!(matches!(next.token, Token::Var));
        
        let primary = self.parse_primary(ctx)?;
        if !primary.is_lvalue() {
            return Err(ParserError::new(ErrorKind::InvalidAssignmentLHS));
        }
        
        let assign_op = self.peek_next_assignment_op()?;
        if assign_op.is_none() {
            ctx.set_end(self.peek().unwrap());
            return Err(ParserError::new(ErrorKind::ExpectedVarAssignment));
        }
        
        let rhs_expr = self.parse_expr(ctx)?;
        
        ctx.pop_extend();
        Ok(Expr::assignment(primary, assign_op.unwrap(), rhs_expr, true))
    }
    
    // Helper to see if the next token is an assignment operator and if so, which one it is.
    fn peek_next_assignment_op(&mut self) -> Result<Option<Option<BinaryOp>>, ParserError> {
        let next = self.peek()?;
        let op = match next.token {
            Token::OpAssign    => None,
            Token::OpAddAssign => Some(BinaryOp::Add),
            Token::OpSubAssign => Some(BinaryOp::Sub),
            Token::OpMulAssign => Some(BinaryOp::Mul),
            Token::OpDivAssign => Some(BinaryOp::Div),
            Token::OpModAssign => Some(BinaryOp::Mod),
            Token::OpAndAssign => Some(BinaryOp::BitAnd),
            Token::OpOrAssign  => Some(BinaryOp::BitOr),
            Token::OpXorAssign => Some(BinaryOp::BitXor),
            Token::OpLShiftAssign => Some(BinaryOp::LShift),
            Token::OpRShiftAssign => Some(BinaryOp::RShift),
            
            _ => return Ok(None),
        };
        
        Ok(Some(op))
    }
    
    /*
        Object Constructor syntax:
        
        object-constructor ::= "{" member-initializer ( "," member-initializer )* "}" ;
        member-initializer ::= ( IDENTIFIER | "[" primary "]" ) ":" expression ;
    
    */
    fn parse_object_constructor(&mut self, ctx: &mut ErrorContext) -> Result<ObjectConstructor, ParserError> {
        ctx.push();
        
        unimplemented!();
        
        ctx.pop_extend();
    }
    
    /*
        Primary expression syntax:
        
        primary ::= atom ( access | subscript | invocation | object-constructor )* ;
        subscript ::= "[" expression "]" ;
        access ::= "." IDENTIFIER ;
        invocation ::= "(" ... ")" ;  (* WIP *)
        object-constructor ::= "{" member-initializer ( "," member-initializer )* "}" ;
    */
    fn parse_primary(&mut self, ctx: &mut ErrorContext) -> Result<Primary, ParserError> { 
        ctx.push();
        
        let mut primary = Primary::new(self.parse_atom(ctx)?);
        
        loop {
            match self.peek()?.token {
                
                // access ::= "." IDENTIFIER ;
                Token::OpAccess => {
                    ctx.push();
                    ctx.set_start(&self.advance().unwrap());
                    
                    let next = self.advance()?;
                    ctx.set_end(&next);
                    
                    if let Token::Identifier(name) = next.token {
                        primary.push_access(name.as_str());
                    } else {
                        return Err(ParserError::new(ErrorKind::ExpectedAccessIdentifier));
                    }
                    
                    ctx.pop_extend();
                },
                
                // subscript ::= "[" expression "]" ;
                Token::OpenSquare => {
                    ctx.push();
                    ctx.set_start(&self.advance().unwrap());
                    
                    let index_expr = self.parse_expr(ctx)?;
                    
                    let next = self.advance()?;
                    ctx.set_end(&next);
                    
                    if matches!(next.token, Token::CloseSquare) {
                        primary.push_indexing(index_expr);
                    } else {
                        return Err(ParserError::new(ErrorKind::ExpectedIndexingClose));
                    }
                    
                    ctx.pop_extend();
                }
                
                // invocation ::= "(" ... ")" ;  (* WIP *)
                Token::OpenParen => {
                    unimplemented!()
                }
                
                // object-constructor ::= "{" ...
                Token::OpenBrace => {
                    ctx.push();
                    ctx.set_start(&self.advance().unwrap());
                    
                    let obj_ctor = self.parse_object_constructor(ctx)?;
                    
                    let next = self.advance()?;
                    ctx.set_end(&next);
                    
                    if matches!(next.token, Token::CloseParen) {
                        primary.push_construct(obj_ctor);
                    } else {
                        return Err(ParserError::new(ErrorKind::ExpectedObjectCtorClose));
                    }
                    
                    ctx.pop_extend();
                }
                
                _ => break,
            };
        }
    
        ctx.pop();
        Ok(primary)
    }
    
    // atom ::= LITERAL | IDENTIFIER | "(" expression ")" ;
    fn parse_atom(&mut self, ctx: &mut ErrorContext) -> Result<Atom, ParserError> { 
        ctx.push();
        
        let next = self.advance()?;
        ctx.set_start(&next);
        
        let atom = match next.token {
            // LITERAL
            Token::Nil => Atom::Nil,
            Token::True => Atom::BooleanLiteral(true),
            Token::False => Atom::BooleanLiteral(false),
            Token::IntegerLiteral(value) => Atom::IntegerLiteral(value),
            Token::FloatLiteral(value) => Atom::FloatLiteral(value),
            // TODO string literals
            
            // IDENTIFIER
            Token::Identifier(ref name) => Atom::identifier(name.as_str()),
            
            // "(" expression ")"
            Token::OpenParen => {
                
                let inner_expr = self.parse_expr(ctx)?;
                
                let next = self.advance()?;
                ctx.set_end(&next);
                
                if !matches!(next.token, Token::CloseParen) {
                    return Err(ParserError::new(ErrorKind::ExpectedGroupClose));
                }
                
                return Ok(Atom::group(inner_expr))
            },
            
            _ => { 
                return Err(ParserError::new(ErrorKind::ExpectedAtom))
            },
        };
        
        ctx.pop_extend();
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