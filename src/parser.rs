mod errors;
mod tests;

pub mod expr;
pub mod stmt;
pub mod primary;
pub mod assign;
pub mod operator;
pub mod structs;

pub use errors::{ParserError, ContextFrame};

use log::debug;

use crate::source::ModuleSource;
use crate::lexer::{TokenMeta, Token, LexerError};
use crate::runtime::strings::{StringInterner, InternSymbol};

use expr::{Expr, ExprVariant};
use stmt::{Stmt, StmtVariant, Label};
use primary::{Primary, Atom};
use assign::{Assignment, LValue, Declaration, DeclType};
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
    
    fn advance(&mut self) -> InternalResult<TokenMeta> {
        let next = self.next.take()
            .or_else(|| self.tokens.next());
        
        if let Some(result) = next {
            Ok(result?)
        } else {
            Err(ErrorKind::EndofTokenStream.into())
        }
    }
    
    fn peek(&mut self) -> InternalResult<&TokenMeta> {
        if self.next.is_none() {
            self.next = self.tokens.next();
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
                debug!("parsing stmt at index {}...", next.span.index);
            },
        }
        
        let result = match self.parse_stmt_variant(&mut ctx) {
            Ok(stmt) => {
                debug!("parser: {:?}", stmt); 
                Ok(stmt)
            },
            Err(err) => {
                debug!("{:#?}", ctx);
                let error = ParserError::from_prototype(err, ctx);
                debug!("parser error: {:?}\ncontext: {:?}\nsymbol: {:?}", 
                    error.kind(), error.context(), 
                    error.debug_symbol(),
                );
                
                debug!("sync to next stmt...");
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
    
    // the top of the recursive descent stack for expressions
    fn parse_expr_variant(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        let next = self.peek()?;
        
        if let Token::Let | Token::Var = next.token {
            self.parse_vardecl_expr(ctx)
        } else {
            self.parse_assignment_expr(ctx)
        }
    }
    
    /*
        Assignment Expression syntax:
        
        lvalue ::= identifier | primary index-access | primary member-access ;

        lvalue-expression ::= lvalue | lvalue-list | "(" lvalue ")" ;   (* basically just lvalues, and tuples of lvalues *)
        lvalue-list ::= lvalue-expression ( "," lvalue-expression )* ;

        lvalue-annotated ::= lvalue-expression ( ":" type-expression )? ; 

        assignment-op ::= "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" ;
        assignment-expression ::= lvalue-annotated assignment-op expression ;
    */
    
    fn parse_assignment_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        
        let expr = self.parse_tuple_expr(ctx)?;
        
        let next = self.peek()?;
        if let Some(op) = Self::which_assignment_op(&next.token) {
            // consume assign_op token
            ctx.push_continuation(ContextTag::AssignmentExpr);
            ctx.set_end(&self.advance().unwrap());
            
            // LHS of assignment has to be an lvalue
            let lhs = LValue::try_from(expr).map_err(|_| ErrorPrototype::from(ErrorKind::InvalidAssignment))?;
            let rhs = self.parse_expr_variant(ctx)?;
            
            ctx.pop_extend();
            
            let op = op.map(|op| op.into());
            let assign = Box::new(Assignment { lhs, op, rhs });
            return Ok(ExprVariant::Assignment(assign));
        }
        
        Ok(expr)
    }
    
    /*
        declaration-expression ::= ( "let" | "var" ) assignment-expression ;
    */
    fn parse_vardecl_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        ctx.push(ContextTag::VarDeclExpr);
        
        let next = self.advance()?;
        ctx.set_end(&next);
        
        let decl = match next.token {
            Token::Let => DeclType::Immutable,
            Token::Var => DeclType::Mutable,
            _ => panic!("parse_vardecl_expr() called but next token was neither let or var"),
        };
        
        let expr = self.parse_tuple_expr(ctx)?;
        let lhs = LValue::try_from(expr).map_err(|_| ErrorPrototype::from(ErrorKind::InvalidAssignment))?;
        
        // check for and consume "="
        let next = self.advance()?;
        ctx.set_end(&next);
        
        if let Some(op) = Self::which_assignment_op(&next.token) {
            if op.is_some() {
                return Err(ErrorKind::InvalidDeclAssignment.into());
            }
        } else {
            return Err(ErrorKind::DeclMissingInitializer.into());
        }
        
        let init = self.parse_expr_variant(ctx)?;
        
        ctx.pop_extend();
        
        let decl = Box::new(Declaration { decl, lhs, init });
        return Ok(ExprVariant::Declaration(decl));
    }
    
    fn parse_tuple_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        
        // descend recursively into binops
        let mut first_expr = Some(self.parse_binop_expr(ctx)?); // might be taken into tuple later
        
        // check for tuple constructor
        let mut tuple_exprs = Vec::<Expr>::new();
        loop {
            let next = self.peek()?;
            if !matches!(next.token, Token::Comma) {
                break;
            }
            
            if let Some(first_expr) = first_expr.take() {
                ctx.push_continuation(ContextTag::Expr);  // retroactivly get debug symbol
                let symbol = ctx.frame().as_debug_symbol().unwrap();
                ctx.pop_extend();
                
                tuple_exprs.push(Expr::new(first_expr, symbol));
                
                ctx.push_continuation(ContextTag::TupleCtor); // enter the tuple context
            }
            
            ctx.set_end(&self.advance().unwrap()); // consume comma
            
            ctx.push(ContextTag::Expr);
            let next_expr = self.parse_binop_expr(ctx)?;
            let symbol = ctx.frame().as_debug_symbol().unwrap();
            ctx.pop_extend();
            
            tuple_exprs.push(Expr::new(next_expr, symbol));
        }
        
        if let Some(expr) = first_expr {
            Ok(expr)
        } else {
            ctx.pop_extend(); // pop the TupleCtor context frame
            Ok(ExprVariant::Tuple(tuple_exprs))
        }
    }
    
    /*
        Binary operator syntax:
        
        operand[1] ::= unary ;
        operand[8] ::= comparison ;
        operand[N] ::= operand[N-1] ( OPERATOR[N] operand[N-1] )* ;
    */
    fn parse_binop_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        self.parse_binop_expr_levels(ctx, PRECEDENCE_START)
    }
    
    fn parse_binop_expr_levels(&mut self, ctx: &mut ErrorContext, level: Precedence) -> InternalResult<ExprVariant> {
        if level == PRECEDENCE_END {
            return self.parse_unary_expr(ctx);  // exit binop precedence recursion
        }
        
        let mut expr = self.parse_binop_expr_levels(ctx, level - 1)?;
        
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
            
            let rhs_expr = self.parse_binop_expr_levels(ctx, level - 1)?;
            
            expr = ExprVariant::BinaryOp(binary_op.into(), Box::new((expr, rhs_expr)));
            
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
            return Ok(ExprVariant::UnaryOp(unary_op.into(), Box::new(expr)));
        }
        
        self.parse_primary_expr(ctx)
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
        Here we parse all the things that are tighter binding than either unary or binary operator expressions.
        We look for anything that can be immediately identified from the next token, or else fall back to a primary expression.
    */
    fn parse_primary_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        let expr = match self.peek()?.token {
            Token::Class => unimplemented!(),
            Token::Fun => unimplemented!(),
            Token::If => unimplemented!(),
            Token::Begin | Token::Label(..) => self.parse_block_expr(ctx)?,
            
            // Token::OpenBrace => Ok(Expr::ObjectCtor(self.parse_object_constructor(ctx)?)),
            
            _ => ExprVariant::Primary(Box::new(self.parse_primary(ctx)?)),
        };
        Ok(expr)
    }
    
    fn parse_block_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprVariant> {
        ctx.push(ContextTag::BlockExpr);
        
        // // check for label
        // let label = 
        //     if let Token::Label(label) = self.peek()?.token {
        //         self.interner.
        //     }
        //     else { None };
        
        
        
        unimplemented!()
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
                        primary.push_access_attr(InternSymbol::from_str(name.as_str(), self.interner))
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
                Token::Identifier(name) => {
                    let name = InternSymbol::from_str(name.as_str(), self.interner);
                    Atom::Identifier(name)
                },
                
                // Literals
                Token::Nil   => Atom::Nil,
                Token::True  => Atom::BooleanLiteral(true),
                Token::False => Atom::BooleanLiteral(false),
                
                Token::IntegerLiteral(value) => Atom::IntegerLiteral(value),
                Token::FloatLiteral(value)   => Atom::FloatLiteral(value),
                Token::StringLiteral(value)   => {
                    let value = InternSymbol::from_str(value.as_str(), self.interner);
                    Atom::StringLiteral(value)
                },
                
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
        
        let expr = self.parse_expr_variant(ctx)?;
        
        // Consume and check closing paren
        let next = self.advance()?;
        ctx.set_end(&next);
        if !matches!(next.token, Token::CloseParen) {
            return Err(ErrorKind::ExpectedCloseParen.into());
        }
        
        ctx.pop_extend();
        Ok(Atom::Group(Box::new(expr)))
    }
    
}
