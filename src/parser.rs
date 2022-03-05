mod errors;
mod tests;

pub mod expr;
pub mod stmt;
pub mod primary;
pub mod operator;
pub mod structs;

pub use errors::{ParserError, ContextFrame};

use log;

use crate::source::ModuleSource;
use crate::runtime::data::StringInterner;
use crate::lexer::{TokenMeta, Token, LexerError};
use crate::debug::symbol::DebugSymbol;

use expr::{Expr, ExprVariant};
use stmt::{Stmt, StmtVariant};
use primary::{Primary, Atom};
use operator::{UnaryOp, BinaryOp, Precedence, PRECEDENCE_START, PRECEDENCE_END};
use structs::{ObjectConstructor};
use errors::{ErrorPrototype, ErrorKind, ErrorContext, ContextTag};




// Recursive descent parser

pub struct Parser<'m, 'h, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    module: &'m ModuleSource,
    interner: &'h mut StringInterner,
    tokens: T,
    next: Option<Result<TokenMeta, LexerError>>,
}

impl<'m, T> Iterator for Parser<'m, '_, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    type Item = Result<Stmt, ParserError<'m>>;
    fn next(&mut self) -> Option<Self::Item> { self.next_stmt() }
}

type InternalResult<T> = Result<T, ErrorPrototype>;

impl<'m, 'h, T> Parser<'m, 'h, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    
    pub fn new(module: &'m ModuleSource, interner: &'h mut StringInterner, tokens: T) -> Self {
        Parser {
            module,
            interner,
            tokens,
            next: None,
        }
    }
    
    fn next_token(&mut self) -> Option<Result<TokenMeta, LexerError>> {
        self.tokens.next()
    }
    
    fn advance(&mut self) -> InternalResult<TokenMeta> {
        let next = self.next.take()
            .or_else(|| self.next_token());
        
        if let Some(result) = next {
            result.map_err(|err| ErrorPrototype::from(ErrorKind::LexerError).caused_by(err))
        } else {
            Err(ErrorKind::EndofTokenStream.into())
        }
    }
    
    fn peek(&mut self) -> InternalResult<&TokenMeta> {
        if self.next.is_none() {
            self.next = self.next_token();
            if self.next.is_none() {
                return Err(ErrorKind::EndofTokenStream.into());
            }
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
    
    pub fn next_stmt(&mut self) -> Option<Result<Stmt, ParserError<'m>>> {
        let mut ctx = ErrorContext::new(self.module, ContextTag::TopLevel);
        
        // check stop conditions
        match self.peek() {
            Err(error) if matches!(error.kind(), ErrorKind::EndofTokenStream) => return None,
            Err(error) => return {
                let error = ParserError::from_prototype(error, ctx);
                Some(Err(error))
            },
            
            Ok(next) if matches!(next.token, Token::EOF) => return None,
            Ok(next) => {
                log::info!("parsing stmt at index {}...", next.span.index);
            },
        }
        
        let result = match self.parse_stmt_variant(&mut ctx) {
            Ok(stmt) => {
                log::debug!("parser: {:?}", stmt); 
                Ok(stmt)
            },
            Err(err) => {
                log::debug!("{:#?}", ctx);
                
                let error = ParserError::from_prototype(err, ctx);
                log::info!("parser error: {:?}\ncontext: {:?}\nsymbol: {:?}", 
                    error.kind(), error.context(), 
                    error.debug_symbol(),
                );
                
                log::info!("sync to next stmt...");
                let mut ctx = ErrorContext::new(self.module, ContextTag::Sync);
                self.synchronize_stmt(&mut ctx);
                
                Err(error)
            },
        };
        Some(result)
    }
    
    // If we hit an error we need to synchronize back to a likely-valid state before we continue parsing again
    // To do this, just keep discarding tokens until we think we're at the start of a new statement
    fn synchronize_stmt(&mut self, ctx: &mut ErrorContext) {
        // Check for either: a token that only appears at the start of a new statement
        // OR try to parse an expression. If we can do it without errors, assume we're in a good state. The expression can be discarded.
        
        loop {

            let next = match self.peek() {
                // no more tokens...
                Err(error) if matches!(error.kind(), ErrorKind::EndofTokenStream) => break,
                
                // skip errors
                Err(..) => {
                    self.advance().unwrap_err();
                    continue;
                },
                Ok(token) => token,
            };

            if matches!(next.token, 
                Token::EOF | Token::Semicolon | Token::Var | 
                Token::While  | Token::Do | Token::For | 
                Token::Continue | Token::Break | Token::Return | 
                Token::Echo) {
                
                break;
            }

            if self.parse_expr_variant(ctx).is_ok() {
                break;
            }

        }
    }
    
    /*** Statement Parsing ***/
    
    fn parse_stmt_variant(&mut self, ctx: &mut ErrorContext) -> InternalResult<Stmt> {
        // skip statement separators
        while let Token::Semicolon = self.peek()?.token {
            self.advance()?;
        }
        
        ctx.push(ContextTag::Stmt);
        
        let stmt = match self.peek()?.token {
            Token::Var => unimplemented!(),
            Token::While => unimplemented!(),
            Token::Do => unimplemented!(),
            Token::For => unimplemented!(),
            Token::Continue => unimplemented!(),
            Token::Break => unimplemented!(),
            Token::Return => unimplemented!(),
            Token::Echo => {
                ctx.set_end(&self.advance().unwrap());
                StmtVariant::Echo(self.parse_expr_variant(ctx)?)
            },
            _ => StmtVariant::Expression(self.parse_expr_variant(ctx)?),
        };
        
        let symbol = ctx.frame().as_debug_symbol().unwrap();
        
        ctx.pop();
        Ok(Stmt::new(stmt, symbol))
    }
    
    /*** Expression Parsing ***/
    
    fn parse_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
        ctx.push(ContextTag::Expr);
        
        let variant = self.parse_expr_variant(ctx)?;
        let symbol = ctx.frame().as_debug_symbol().unwrap();
        
        ctx.pop_extend();
        Ok(Expr::new(variant, symbol))
    }
    
    fn parse_expr_variant(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        let first_expr = self.parse_inner_expr(ctx)?;
        
        // check for tuple constructor
        let mut rest_exprs = Vec::<Expr>::new();
        loop {
            let next = self.peek()?;
            if !matches!(next.token, Token::Comma) {
                break;
            }
            
            if rest_exprs.is_empty() {
                ctx.push_continuation(ContextTag::TupleCtor);
            }
            ctx.set_end(&self.advance().unwrap()); // consume comma
            
            let next_expr = self.parse_inner_expr(ctx)?;
            rest_exprs.push(next_expr);
        }
        
        if rest_exprs.is_empty() {
            Ok(first_expr.take_variant())
        } else {
            rest_exprs.insert(0, first_expr);
            ctx.pop_extend(); // pop the TupleCtor context frame
            
            Ok(ExprVariant::Tuple(rest_exprs))
        }
    }
    
    // parse an expression that cannot be a tuple (without grouping)
    fn parse_inner_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
        ctx.push(ContextTag::Expr);
        
        let variant = self.parse_inner_expr_variant(ctx)?;
        let symbol = ctx.frame().as_debug_symbol().unwrap();
        
        ctx.pop_extend();
        Ok(Expr::new(variant, symbol))
    }
    
    fn parse_inner_expr_variant(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        self.parse_assignment_expr(ctx)  // the top of the recursive descent stack for expressions
    }
    
    /*
        Assignment Expression syntax:
        
        assignment-expression ::= assignment-target ( "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" ) expression ;
        assignment-target ::= ( "var" )? lvalue ; 
    */
    
    fn parse_assignment_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        
        // descend recursively
        let expr = self.parse_binop_expr(ctx, PRECEDENCE_START)?;
        
        let next = self.peek()?;
        if let Some(assign_op) = Self::which_assignment_op(&next.token) {
            // consume assign_op token
            ctx.push_continuation(ContextTag::AssignmentExpr);
            ctx.set_end(&self.advance().unwrap());
            
            // LHS of assignment has to be an lvalue
            let lhs = match expr {
                ExprVariant::Primary(lhs) if lhs.is_lvalue() => lhs,
                _ => return Err(ErrorKind::InvalidAssignmentLHS.into()),
            };

            let rhs_expr = self.parse_inner_expr_variant(ctx)?;
            
            ctx.pop_extend();
            return Ok(ExprVariant::assignment(*lhs, assign_op.map(|op| op.into()), rhs_expr));
        }
        
        Ok(expr)
    }
    
    /*
        Binary operator syntax:
        
        operand[1] ::= unary ;
        operand[8] ::= comparison ;
        operand[N] ::= operand[N-1] ( OPERATOR[N] operand[N-1] )* ;
    */
    fn parse_binop_expr(&mut self, ctx: &mut ErrorContext, level: Precedence) -> InternalResult<ExprVariant> {
        if level == PRECEDENCE_END {
            return self.parse_unary_expr(ctx);  // exit binop precedence recursion
        }
        
        let mut expr = self.parse_binop_expr(ctx, level - 1)?;
        
        loop {
            let next = self.peek()?;
            let binary_op = Self::which_binary_op(&next.token);
            
            if binary_op.is_none() {
                break;
            }
            
            let binary_op = binary_op.unwrap();
            if binary_op.precedence_level() != level {
                break;
            }
            
            ctx.push_continuation(ContextTag::BinaryOpExpr);
            ctx.set_end(&self.advance().unwrap()); // consume binary_op token
            
            let rhs_expr = self.parse_binop_expr(ctx, level - 1)?;
            
            expr = ExprVariant::binary_op(binary_op.into(), expr, rhs_expr);
            
            ctx.pop_extend();
        }
        
        Ok(expr)
    }
    
    /*
        Unary operator syntax:
        
        unary-expression ::= ( "-" | "+" | "not" ) unary | primary ;
    */
    fn parse_unary_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        let next = self.peek()?;
        if let Some(unary_op) = Self::which_unary_op(&next.token) {
            ctx.push(ContextTag::UnaryOpExpr);
            ctx.set_start(&self.advance().unwrap()); // consume unary_op token
            
            let expr = self.parse_primary_expr(ctx)?;
            
            ctx.pop_extend();
            return Ok(ExprVariant::unary_op(unary_op.into(), expr));
        }
        
        self.parse_primary_expr(ctx)
    }
    
    /*
        Here we parse all the things that are tighter binding than either unary or binary operator expressions.
        We look for anything that can be immediately identified from the next token, or else fall back to a primary expression.
    */
    fn parse_primary_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        match self.peek()?.token {
            Token::Class => unimplemented!(),
            Token::Fun => unimplemented!(),
            Token::If => unimplemented!(),
            Token::Begin => unimplemented!(),
            
            // Token::OpenBrace => Ok(Expr::ObjectCtor(self.parse_object_constructor(ctx)?)),
            
            _ => Ok(ExprVariant::primary(self.parse_primary(ctx)?)),
        }
    }

    fn which_unary_op(token: &Token) -> Option<UnaryOp> {
        let op = match token {
            Token::OpAdd => UnaryOp::Pos,
            Token::OpSub => UnaryOp::Neg,
            Token::OpInv => UnaryOp::Inv,
            Token::Not   => UnaryOp::Not,
            
            _ => return None,
        };
        
        Some(op)
    }
    
    fn which_binary_op(token: &Token) -> Option<BinaryOp> {
        let op = match token {
            Token::OpMul => BinaryOp::Mul,
            Token::OpDiv => BinaryOp::Div,
            Token::OpMod => BinaryOp::Mod,
            Token::OpAdd => BinaryOp::Add,
            Token::OpSub => BinaryOp::Sub,
            Token::OpLShift => BinaryOp::LShift,
            Token::OpRShift => BinaryOp::RShift,
            Token::OpAnd => BinaryOp::BitAnd,
            Token::OpXor => BinaryOp::BitXor,
            Token::OpOr => BinaryOp::BitOr,
            Token::OpLT => BinaryOp::LT,
            Token::OpGT => BinaryOp::GT,
            Token::OpLE => BinaryOp::LE,
            Token::OpGE => BinaryOp::GE,
            Token::OpEQ => BinaryOp::EQ,
            Token::OpNE => BinaryOp::NE,
            Token::And => BinaryOp::And,
            Token::Or => BinaryOp::Or,
            
            _ => return None,
        };
        
        Some(op)
    }
    
    // Helper to see if the a token is an assignment operator and if so, which one it is.
    fn which_assignment_op(token: &Token) -> Option<Option<BinaryOp>> {
        let op = match token {
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
            
            _ => return None,
        };
        
        Some(op)
    }
    
    /*
        Object Constructor syntax:
        
        object-constructor ::= "{" member-initializer ( "," member-initializer )* "}" ;
        member-initializer ::= ( IDENTIFIER | "[" primary "]" ) ":" expression ;
    
    */
    fn parse_object_constructor(&mut self, ctx: &mut ErrorContext) -> InternalResult<ObjectConstructor> {
        ctx.push(ContextTag::ObjectCtor);
        
        let next = self.advance().unwrap();
        ctx.set_start(&self.advance().unwrap());
        debug_assert!(matches!(next.token, Token::OpenBrace));
        
        unimplemented!();
        
        // let next = self.advance()?;
        // ctx.set_end(&next);
        
        // if !matches!(next.token, Token::CloseParen) {
        //     return Err(ParserError::new(ErrorKind::ExpectedCloseBrace, ctx.context()));
        // }
        
        // ctx.pop_extend();
    }
    
    /*
        Primary expression syntax:
        
        primary ::= atom ( access | subscript | invocation | object-constructor )* ;
        subscript ::= "[" expression "]" ;
        access ::= "." IDENTIFIER ;
        invocation ::= "(" ... ")" ;  (* WIP *)
        object-constructor ::= "{" member-initializer ( "," member-initializer )* "}" ;
    */
    fn parse_primary(&mut self, ctx: &mut ErrorContext) -> InternalResult<Primary> { 
        ctx.push(ContextTag::PrimaryExpr);
        
        let mut primary = Primary::new(self.parse_atom(ctx)?);
        
        loop {
            let next = self.peek()?;
            match next.token {
                
                // access ::= "." IDENTIFIER ;
                Token::OpAccess => {
                    ctx.push(ContextTag::MemberAccess);
                    ctx.set_start(&self.advance().unwrap());
                    
                    let next = self.advance()?;
                    ctx.set_end(&next);
                    
                    if let Token::Identifier(name) = next.token {
                        primary.push_access_member(name.as_str(), self.interner);
                    } else {
                        return Err(ErrorKind::ExpectedIdentifier.into());
                    }
                    
                    ctx.pop_extend();
                },
                
                // subscript ::= "[" expression "]" ;
                Token::OpenSquare => {
                    ctx.push(ContextTag::IndexAccess);
                    ctx.set_start(&self.advance().unwrap());
                    
                    let index_expr = self.parse_expr(ctx)?;
                    
                    let next = self.advance()?;
                    ctx.set_end(&next);
                    
                    if matches!(next.token, Token::CloseSquare) {
                        primary.push_access_index(index_expr);
                    } else {
                        return Err(ErrorKind::ExpectedCloseSquare.into());
                    }
                    
                    ctx.pop_extend();
                }
                
                // Invocation is a special case. 
                // The parens containing the argument list are not allowed to be on a separate line
                // Lua has a similar issue, but doesn't have tuples so it isn't as big a problem there
                // By checking next.newline we ensure that a () on the next line is properly parsed as a tuple
                
                // invocation ::= "(" ... ")" ;  (* WIP *)
                Token::OpenParen if !next.newline => {
                    unimplemented!()
                }
                
                // object-constructor ::= "{" ... "}"
                Token::OpenBrace => {
                    ctx.push(ContextTag::ObjectCtor);
                    ctx.set_start(&self.advance().unwrap());
                    
                    let obj_ctor = self.parse_object_constructor(ctx)?;
                    
                    let next = self.advance()?;
                    ctx.set_end(&next);
                    
                    if matches!(next.token, Token::CloseParen) {
                        primary.push_construct(obj_ctor);
                    } else {
                        return Err(ErrorKind::ExpectedCloseBrace.into());
                    }
                    
                    ctx.pop_extend();
                }
                
                _ => break,
            };
        }
    
        ctx.pop_extend();
        Ok(primary)
    }
    
    // atom ::= LITERAL | IDENTIFIER | "(" expression ")" ;
    fn parse_atom(&mut self, ctx: &mut ErrorContext) -> InternalResult<Atom> { 
        
        if let Token::OpenParen = self.peek()?.token {
            Ok(self.parse_group_expr(ctx)?)  // Groups
            
        } else { 
            ctx.push(ContextTag::Atom);
            
            let next = self.advance().unwrap();
            ctx.set_start(&next);
            
            let atom = match next.token {
                // Identifiers
                Token::Identifier(name) => Atom::identifier(name.as_str(), self.interner),
                Token::Global | Token::Upval => {
                    let scope = next.token;
                    let next = self.advance()?;
                    ctx.set_end(&next);
                    
                    if let Token::Identifier(name) = next.token {
                        match scope {
                            Token::Global => Atom::global_identifier(name.as_str(), self.interner),
                            Token::Upval => Atom::upval_identifier(name.as_str(), self.interner),
                            _ => unreachable!(),
                        }
                        
                    } else {
                        return Err(ErrorKind::ExpectedIdentifier.into())
                    }
                },
                
                // Literals
                Token::Nil   => Atom::Nil,
                Token::True  => Atom::boolean(true),
                Token::False => Atom::boolean(false),
                
                Token::IntegerLiteral(value) => Atom::integer(value),
                Token::FloatLiteral(value)   => Atom::float(value),
                Token::StringLiteral(value)   => Atom::string_literal(value.as_str(), self.interner),
                
                _ => { 
                    return Err(ErrorKind::ExpectedStartOfExpr.into())
                },
            };
            
            ctx.pop_extend();
            Ok(atom)
        }
    }
    
    fn parse_group_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Atom> {
        ctx.push(ContextTag::Group);
        
        let next = self.advance().unwrap(); // consume the "("
        ctx.set_start(&next);
        debug_assert!(matches!(next.token, Token::OpenParen));
        
        // Check for the empty tuple
        let next = self.peek()?;
        if let Token::CloseParen = next.token {
            ctx.set_end(&self.advance().unwrap());
            ctx.pop_extend();
            return Ok(Atom::EmptyTuple);
        }
        
        // need to duplicate some of the tuple parsing logic here because the only way to
        // identify a single-element tuple is by a comma followed by a closing paren.
        // and parse_expr_variant() only deals with naked tuples
        
        let mut expr = Some(self.parse_inner_expr(ctx)?); // may be taken by tuple_expr later
        let mut tuple_exprs = Vec::<Expr>::new();
        
        // loop through expressions in case this is a tuple
        loop {
            let next = self.advance()?;
            ctx.set_end(&next);
            
            match next.token {
                
                // end of group
                Token::CloseParen => break,
                
                // single-element tuples
                Token::Comma => if tuple_exprs.is_empty() {
                    tuple_exprs.push(expr.take().unwrap());
                    
                    // comma followed by paren
                    if let Token::CloseParen = self.peek()?.token {
                        continue;
                    }
                }
                
                _ => {
                    return Err(ErrorKind::ExpectedCloseParen.into());
                }
            }
            
            let next_expr = self.parse_inner_expr(ctx)?;
            tuple_exprs.push(next_expr);
        }
        
        let group = if tuple_exprs.is_empty() {
            Atom::group(expr.unwrap().take_variant())
        } else {
            Atom::group(ExprVariant::Tuple(tuple_exprs))
        };
        
        ctx.pop_extend();
        Ok(group)
    }
    
}
