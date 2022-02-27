mod errors;
mod tests;

pub mod expr;
pub mod primary;
pub mod operator;
pub mod structs;

pub use errors::{ParserError, ContextFrame};

use crate::source::ModuleSource;
use crate::runtime::data::StringInterner;
use crate::lexer::{TokenMeta, Token, LexerError};
use crate::debug::symbol::DebugSymbol;

use expr::{Expr, ExprVariant};
use primary::{Primary, Atom};
use operator::{UnaryOp, BinaryOp, OpLevel, OP_LEVEL_START, OP_LEVEL_END};
use structs::{ObjectConstructor};
use errors::{ErrorPrototype, ErrorKind, ErrorContext, ContextTag};




// Recursive descent parser

pub struct Parser<'m, 'h, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    module: &'m ModuleSource,
    interner: &'h mut StringInterner,
    tokens: T,
    next: Option<Result<TokenMeta, LexerError>>,
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
    
    fn next_token(&mut self) -> Result<TokenMeta, LexerError> {
        // should never run out of tokens as we should always get EOF first
        self.tokens.next().expect("unexpected end of token sequence")
    }
    
    fn advance(&mut self) -> InternalResult<TokenMeta> {
        self.next.take()
            .unwrap_or_else(|| self.next_token())
            .map_err(|err| ErrorPrototype::caused_by(
                Box::new(err), ErrorKind::LexerError
            ))
    }
    
    fn peek(&mut self) -> InternalResult<&TokenMeta> {
        if self.next.is_none() {
            self.next = Some(self.next_token());
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
    
    // temporary top level, will change when statement parsing is added
    // Note: the value returned from this method no longer needs the interner 'h
    // but the compiler can't guess that, so we have to be explicit about the lifetimes here
    pub fn placeholder_toplevel(&mut self) -> Result<Expr, ParserError<'m>> { 
        let mut ctx = ErrorContext::new(self.module, ContextTag::Statement);
        
        match self.parse_expr(&mut ctx) {
            Ok(expr) => Ok(expr),
            Err(err) => {
                // TODO synchronize
            
                Err(ParserError::from_prototype(err, ctx))
            },
        }
    }
    
    /*** Expression Parsing ***/
    
    fn parse_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
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
            Ok(first_expr)
        } else {
            rest_exprs.insert(0, first_expr);
            let tuple = ExprVariant::Tuple(rest_exprs);
            let symbol = DebugSymbol::from(ctx.frame());
            
            ctx.pop_extend(); // pop the TupleCtor context frame
            Ok(Expr::new(tuple, symbol))
        }
    }
    
    // parse an expression that cannot be a tuple
    fn parse_inner_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
        ctx.push(ContextTag::Expr);
        
        let variant = self.parse_expr_variant(ctx)?;
        let symbol = DebugSymbol::from(ctx.frame());
        
        ctx.pop_extend();
        Ok(Expr::new(variant, symbol))
    }
    
    fn parse_expr_variant(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        self.parse_assignment_expr(ctx)  // the top of the recursive descent stack for expressions
    }
    
    /*
        Assignment Expression syntax:
        
        assignment-expression ::= assignment-target ( "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" ) expression ;
        assignment-target ::= ( "var" )? lvalue ; 
    */
    
    fn parse_assignment_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        
        // descend recursively
        let expr = self.parse_binop_expr(ctx, OP_LEVEL_START)?;
        
        let next = self.peek()?;
        if let Some(assign_op) = Self::which_assignment_op(&next.token) {
            // consume assign_op token
            ctx.push_continuation(ContextTag::AssignmentExpr);
            ctx.set_end(&self.advance().unwrap());
            
            // LHS of assignment has to be an lvalue
            let lhs = match expr {
                ExprVariant::Primary(lhs) if lhs.is_lvalue() => lhs,
                _ => return Err(ErrorPrototype::new(ErrorKind::InvalidAssignmentLHS)),
            };

            let rhs_expr = self.parse_expr_variant(ctx)?;
            
            ctx.pop_extend();
            return Ok(ExprVariant::assignment(*lhs, assign_op, rhs_expr));
        }
        
        Ok(expr)
    }
    
    /*
        Binary operator syntax:
        
        operand[1] ::= unary ;
        operand[8] ::= comparison ;
        operand[N] ::= operand[N-1] ( OPERATOR[N] operand[N-1] )* ;
    */
    fn parse_binop_expr(&mut self, ctx: &mut ErrorContext, level: OpLevel) -> InternalResult<ExprVariant> {
        if level == OP_LEVEL_END {
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
            
            expr = ExprVariant::binary_op(binary_op, expr, rhs_expr);
            
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
            return Ok(ExprVariant::unary_op(unary_op, expr));
        }
        
        self.parse_primary_expr(ctx)
    }
    
    /*
        Here we parse all the things that are tighter binding than either unary or binary operator expressions.
        We look for anything that can be immediately identified from the next token, or else fall back to a primary expression.
    */
    fn parse_primary_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        let next = self.peek()?;
        match next.token {
            Token::Class => unimplemented!(),
            Token::Fun => unimplemented!(),
            Token::If => unimplemented!(),
            
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
            match self.peek()?.token {
                
                // access ::= "." IDENTIFIER ;
                Token::OpAccess => {
                    ctx.push(ContextTag::MemberAccess);
                    ctx.set_start(&self.advance().unwrap());
                    
                    let next = self.advance()?;
                    ctx.set_end(&next);
                    
                    if let Token::Identifier(name) = next.token {
                        primary.push_access_member(name.as_str(), self.interner);
                    } else {
                        return Err(ErrorPrototype::new(ErrorKind::ExpectedIdentifier));
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
                        return Err(ErrorPrototype::new(ErrorKind::ExpectedCloseSquare));
                    }
                    
                    ctx.pop_extend();
                }
                
                // invocation ::= "(" ... ")" ;  (* WIP *)
                Token::OpenParen => {
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
                        return Err(ErrorPrototype::new(ErrorKind::ExpectedCloseBrace));
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
                Token::Global => {
                    let next = self.advance()?;
                    ctx.set_end(&next);
                    
                    if let Token::Identifier(name) = next.token {
                        Atom::global_identifier(name.as_str(), self.interner)
                    } else {
                        return Err(ErrorPrototype::new(ErrorKind::ExpectedIdentifier))
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
                    return Err(ErrorPrototype::new(ErrorKind::ExpectedStartOfExpr))
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
            ctx.pop_extend();
            return Ok(Atom::EmptyTuple);
        }
        
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
                    return Err(ErrorPrototype::new(ErrorKind::ExpectedCloseParen));
                }
            }
            
            let next_expr = self.parse_inner_expr(ctx)?;
            tuple_exprs.push(next_expr);
        }
        
        let group = if tuple_exprs.is_empty() {
            Atom::group(expr.unwrap())
        } else {
            let tuple = ExprVariant::Tuple(tuple_exprs);
            let symbol = DebugSymbol::from(ctx.frame());
            
            Atom::group(Expr::new(tuple, symbol))
        };
        
        ctx.pop_extend();
        Ok(group)
    }
    
    
    // Discards tokens until we reach a statement boundary
    fn synchronize_stmt(&mut self) {
        unimplemented!()
    }
}