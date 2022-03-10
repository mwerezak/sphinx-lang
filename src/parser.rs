use std::collections::VecDeque;

use log::debug;

use crate::source::ModuleSource;
use crate::lexer::{TokenMeta, Token, TokenIndex, LexerError};
use crate::runtime::string_table::StringTable;

use expr::{ExprMeta, Expr};
use stmt::{StmtMeta, Stmt, Label};
use primary::{Primary, Atom, AccessItem};
use assign::{Assignment, LValue, Declaration, DeclType};
use operator::{UnaryOp, BinaryOp, Precedence, PRECEDENCE_START, PRECEDENCE_END};
use structs::{ObjectConstructor};
use errors::{ErrorPrototype, ErrorKind, ErrorContext, ContextTag};


mod errors;
mod tests;

pub mod expr;
pub mod stmt;
pub mod primary;
pub mod assign;
pub mod operator;
pub mod structs;

pub use errors::{ParserError, ContextFrame};


// Recursive descent parser

pub struct Parser<'m, 'h, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    module: &'m ModuleSource,
    interner: &'h mut StringTable,
    tokens: T,
    next: Option<Result<TokenMeta, LexerError>>,
    errors: VecDeque<ErrorPrototype>,
}

impl<'m, T> Iterator for Parser<'m, '_, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    type Item = Result<StmtMeta, ParserError<'m>>;
    fn next(&mut self) -> Option<Self::Item> { self.next_stmt() }
}

type InternalResult<T> = Result<T, ErrorPrototype>;

impl<'m, 'h, I> Parser<'m, 'h, I> where I: Iterator<Item=Result<TokenMeta, LexerError>> {
    
    pub fn new(module: &'m ModuleSource, interner: &'h mut StringTable, tokens: I) -> Self {
        Parser {
            module,
            interner,
            tokens,
            next: None,
            errors: VecDeque::new(),
        }
    }
    
    /// for debugging
    fn current_index(&mut self) -> TokenIndex { self.peek().unwrap().span.index }
    
    /// Helper for methods that can't eagerly return with an error but must collect them into a sequence to return later
    fn append_errors(&mut self, errors: impl Iterator<Item=ErrorPrototype>) -> Option<ErrorPrototype> {
        self.errors.extend(errors);
        self.errors.pop_front()
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
    
    // peek() will consume any errors it encounters
    // this is so that we don't have to do complex map_err() every single time we call self.peek()
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
    
    pub fn next_stmt(&mut self) -> Option<Result<StmtMeta, ParserError<'m>>> {
        let mut ctx = ErrorContext::new(self.module, ContextTag::TopLevel);
        
        if let Some(error) = self.errors.pop_front() {
            return Some(Err(Self::process_error(ctx, error)));
        }
        
        // check stop conditions
        loop {
            match self.peek() {
                Err(error) if matches!(error.kind(), ErrorKind::EndofTokenStream) => return None,
                Err(error) => return {
                    let error = ParserError::from_prototype(error, ctx);
                    Some(Err(error))
                },
                
                Ok(next) if matches!(next.token, Token::EOF) => return None,
                
                // skip statement separators
                Ok(next) if matches!(next.token, Token::Semicolon) => {
                    self.advance().unwrap();
                    continue;
                },
                
                Ok(..) => break,
            }
        }
        
        let result = match self.parse_stmt(&mut ctx) {
            Ok(stmt) => {
                debug!("parser: {:?}", stmt); 
                Ok(stmt)
            },
            Err(err) => {
                let error = Self::process_error(ctx, err);
                
                self.synchronize_stmt(false);
                
                Err(error)
            },
        };
        Some(result)
    }
    
    fn process_error(ctx: ErrorContext<'m>, error: ErrorPrototype) -> ParserError<'m> {
        debug!("{:#?}", ctx);
        
        let error = ParserError::from_prototype(error, ctx);
        
        debug!("parser error: {:?}\ncontext: {:?}\nsymbol: {:?}", 
            error.kind(), error.context(), 
            error.debug_symbol(),
        );
        
        error
    }
    
    // If we hit an error we need to synchronize back to a likely-valid state before we continue parsing again
    // To do this, just keep discarding tokens until we think we're at the start of a new statement
    fn synchronize_stmt(&mut self, expect_end: bool) {
        // Check for either: a token that only appears at the start of a new statement
        // OR try to parse an expression. If we can do it without errors, assume we're in a good state. The expression can be discarded.
        debug!("sync to next stmt...");
        
        let mut ctx = ErrorContext::new(self.module, ContextTag::Sync);
        
        loop {

            let next = match self.peek() {
                // no more tokens...
                Err(error) if matches!(error.kind(), ErrorKind::EndofTokenStream) => break,
                
                // skip errors
                Err(..) => continue,  // peek will consume errors
                
                Ok(token) => token,
            };

            match next.token {
                Token::EOF | Token::Semicolon | Token::Var | 
                Token::While  | Token::Do | Token::For | 
                Token::Continue | Token::Break | Token::Return | 
                Token::Echo 
                    => break,
                
                Token::End if expect_end => break,
                
                _ => if self.parse_expr_variant(&mut ctx).is_ok() {
                    break;
                },
            }

        }
        
        debug!("done.");
    }
    
    /*** Statement Parsing ***/
    
    fn parse_stmt(&mut self, ctx: &mut ErrorContext) -> InternalResult<StmtMeta> {
        // skip statement separators
        while let Token::Semicolon = self.peek()?.token {
            self.advance()?;
        }
        
        debug!("parsing stmt at index {}...", self.current_index());
        
        ctx.push(ContextTag::StmtMeta);
        
        let stmt = self.parse_stmt_variant(ctx)?;
        let symbol = ctx.frame().as_debug_symbol().unwrap();
        
        ctx.pop_extend();
        Ok(StmtMeta::new(stmt, symbol))
    }
    
    fn parse_stmt_variant(&mut self, ctx: &mut ErrorContext) -> InternalResult<Stmt> {
        let stmt = match self.peek()?.token {
            Token::While => unimplemented!(),
            Token::Do => unimplemented!(),
            Token::For => unimplemented!(),
            Token::Echo => {
                ctx.set_end(&self.advance().unwrap());
                Stmt::Echo(self.parse_expr_variant(ctx)?)
            },
            
            // only allowed at the end of a statement list
            Token::Continue | Token::Break | Token::Return => {
                ctx.push(ContextTag::ControlFlow);
                ctx.set_start(&self.advance().unwrap());
                return Err(ErrorKind::ControlFlowOutsideOfBlock.into());
            },
            
            _ => Stmt::Expression(self.parse_expr_variant(ctx)?),
        };
        Ok(stmt)
    }
    
    fn parse_statement_list(&mut self, ctx: &mut ErrorContext) -> InternalResult<Vec<StmtMeta>> {
        ctx.push(ContextTag::StmtList);
        
        let mut suite = Vec::new();
        let mut errors = Vec::new();
        
        debug!("enter stmt list at index {}...", self.current_index());
        
        loop {
            
            let next = self.peek()?;
            if matches!(next.token, Token::End) {
                break;
            }
            
            let stmt = match self.try_parse_control_flow_stmt(ctx) {
                Ok(stmt) => stmt,
                Err(error) => {
                    errors.push(error.with_symbol_from_ctx(&ctx));
                    self.synchronize_stmt(true);
                    continue;
                }
            };
            
            if let Some(stmt) = stmt {
                suite.push(stmt);
                
                let next = self.peek()?;
                if !matches!(next.token, Token::End) {
                    // consume the unexpected token so that it is included in the error message
                    ctx.set_end(&self.advance().unwrap());
                    
                    let error = ErrorPrototype::from(ErrorKind::ExpectedEndAfterControlFlow)
                        .with_symbol_from_ctx(&ctx);
                    
                    errors.push(error);
                }
                
                break;
            }
            
            let stmt = match self.parse_stmt(ctx) {
                Ok(stmt) => stmt,
                Err(error) => {
                    errors.push(error.with_symbol_from_ctx(&ctx));
                    self.synchronize_stmt(true);
                    continue;
                }
            };
            
            suite.push(stmt);
        }
        
        ctx.set_end(&self.advance().unwrap());
        
        debug!("exit stmt list at {}...", self.current_index());
        
        if !errors.is_empty() {
            return Err(self.append_errors(errors.into_iter()).unwrap());
        }
        
        ctx.pop_extend();
        Ok(suite)
    }
    
    fn try_parse_label(&mut self, _ctx: &mut ErrorContext) -> InternalResult<Option<Label>> {
        let next = self.peek()?;
        
        let label = if let Token::Label(..) = next.token {
            let next = self.advance().unwrap();
            
            if let Token::Label(name) = next.token {
                Some(Label::new(self.interner.get_or_intern(name.as_str()).into()))
            } else { unreachable!() }
            
        } else { None };
        Ok(label)
    }
    
    fn try_parse_control_flow_stmt(&mut self, ctx: &mut ErrorContext) -> InternalResult<Option<StmtMeta>> {
        let next = self.peek()?;
        
        let stmt = match next.token {
            Token::Continue => {
                ctx.push(ContextTag::ControlFlow);
                ctx.set_start(&self.advance().unwrap());
                
                let label = self.try_parse_label(ctx)?;
                
                Stmt::Continue(label)
            },
            
            Token::Break => {
                ctx.push(ContextTag::ControlFlow);
                ctx.set_start(&self.advance().unwrap());
                
                let label = self.try_parse_label(ctx)?;
                
                let expr = 
                    if !matches!(self.peek()?.token, Token::End | Token::Semicolon) {
                        Some(self.parse_expr_variant(ctx)?)
                    } else { None };
                
                Stmt::Break(label, expr)
            },
            
            Token::Return => {
                ctx.push(ContextTag::ControlFlow);
                ctx.set_start(&self.advance().unwrap());
                
                let expr = 
                    if !matches!(self.peek()?.token, Token::End | Token::Semicolon) {
                        Some(self.parse_expr_variant(ctx)?)
                    } else { None };
                
                Stmt::Return(expr)
            }
            
            _ => return Ok(None),
        };
        
        let symbol = ctx.frame().as_debug_symbol().unwrap();
        
        ctx.pop_extend();
        Ok(Some(StmtMeta::new(stmt, symbol)))
    }
    
    /*** Expression Parsing ***/
    
    fn parse_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<ExprMeta> {
        ctx.push(ContextTag::ExprMeta);
        
        let variant = self.parse_expr_variant(ctx)?;
        let symbol = ctx.frame().as_debug_symbol().unwrap();
        
        ctx.pop_extend();
        Ok(ExprMeta::new(variant, symbol))
    }
    
    // the top of the recursive descent stack for expressions
    fn parse_expr_variant(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
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
    
    fn parse_assignment_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
        
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
            return Ok(Expr::Assignment(assign));
        }
        
        Ok(expr)
    }
    
    /*
        declaration-expression ::= ( "let" | "var" ) assignment-expression ;
    */
    fn parse_vardecl_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
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
        return Ok(Expr::Declaration(decl));
    }
    
    fn parse_tuple_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
        
        // descend recursively into binops
        let mut first_expr = Some(self.parse_binop_expr(ctx)?); // might be taken into tuple later
        
        // check for tuple constructor
        let mut tuple_exprs = Vec::<ExprMeta>::new();
        loop {
            let next = self.peek()?;
            if !matches!(next.token, Token::Comma) {
                break;
            }
            
            if let Some(first_expr) = first_expr.take() {
                ctx.push_continuation(ContextTag::ExprMeta);  // retroactivly get debug symbol
                let symbol = ctx.frame().as_debug_symbol().unwrap();
                ctx.pop_extend();
                
                tuple_exprs.push(ExprMeta::new(first_expr, symbol));
                
                ctx.push_continuation(ContextTag::TupleCtor); // enter the tuple context
            }
            
            ctx.set_end(&self.advance().unwrap()); // consume comma
            
            ctx.push(ContextTag::ExprMeta);
            let next_expr = self.parse_binop_expr(ctx)?;
            let symbol = ctx.frame().as_debug_symbol().unwrap();
            ctx.pop_extend();
            
            tuple_exprs.push(ExprMeta::new(next_expr, symbol));
        }
        
        if let Some(expr) = first_expr {
            Ok(expr)
        } else {
            ctx.pop_extend(); // pop the TupleCtor context frame
            Ok(Expr::Tuple(tuple_exprs))
        }
    }
    
    /*
        Binary operator syntax:
        
        operand[1] ::= unary ;
        operand[N] ::= operand[N-1] ( OPERATOR[N] operand[N-1] )* ;
    */
    fn parse_binop_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
        self.parse_binop_expr_levels(ctx, PRECEDENCE_START)
    }
    
    fn parse_binop_expr_levels(&mut self, ctx: &mut ErrorContext, level: Precedence) -> InternalResult<Expr> {
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
            
            expr = Expr::BinaryOp(binary_op.into(), Box::new((expr, rhs_expr)));
            
            ctx.pop_extend();
        }
        
        Ok(expr)
    }
    
    /*
        Unary operator syntax:
        
        unary-expression ::= ( "-" | "+" | "not" ) unary | primary ;
    */
    fn parse_unary_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
        let next = self.peek()?;
        if let Some(unary_op) = Self::which_unary_op(&next.token) {
            ctx.push(ContextTag::UnaryOpExpr);
            ctx.set_start(&self.advance().unwrap()); // consume unary_op token
            
            let expr = self.parse_unary_expr(ctx)?;
            
            ctx.pop_extend();
            return Ok(Expr::UnaryOp(unary_op.into(), Box::new(expr)));
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
    fn parse_primary_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
        let expr = match self.peek()?.token {
            Token::Class => unimplemented!(),
            Token::Fun => unimplemented!(),
            Token::If => unimplemented!(),
            Token::Begin | Token::Label(..) => self.parse_block_expr(ctx)?,
            
            // Token::OpenBrace => Ok(ExprMeta::ObjectCtor(self.parse_object_constructor(ctx)?)),
            
            _ => self.parse_primary(ctx)?,
        };
        Ok(expr)
    }
    
    /*
        block-expression ::= ( label )? "begin" ( statement | control-flow | "break" ( label )? expression )* "end" ;  (* break can be supplied a value inside of begin-blocks *)
    */
    fn parse_block_expr(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
        ctx.push(ContextTag::BlockExpr);
        ctx.set_start(self.peek()?);
        
        // check for label
        let block_label = self.try_parse_label(ctx)?;
        
        // consume "begin"
        let next = self.advance()?;
        ctx.set_end(&next);
        if !matches!(next.token, Token::Begin) {
            return Err(ErrorKind::ExpectedItemAfterLabel.into());
        }
        
        let suite = self.parse_statement_list(ctx)?;
        
        // TODO syntatic sugar: if the last statement is an expression statement, it implicitly becomes a "break <expr>"
        
        ctx.pop_extend();
        Ok(Expr::Block(suite, block_label))
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
    fn parse_primary(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> { 
        ctx.push(ContextTag::PrimaryExpr);
        
        let atom = self.parse_atom(ctx)?;
        
        let mut items = Vec::new();
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
                        items.push(AccessItem::Attribute(self.interner.get_or_intern(name.as_str()).into()));
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
                        items.push(AccessItem::Index(index_expr));
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
                    unimplemented!()
                    // ctx.push(ContextTag::ObjectCtor);
                    // ctx.set_start(&self.advance().unwrap());
                    
                    // let obj_ctor = self.parse_object_constructor(ctx)?;
                    
                    // let next = self.advance()?;
                    // ctx.set_end(&next);
                    
                    // if matches!(next.token, Token::CloseParen) {
                    //     primary.push_construct(obj_ctor);
                    // } else {
                    //     return Err(ErrorKind::ExpectedCloseBrace.into());
                    // }
                    
                    // ctx.pop_extend();
                }
                
                _ => break,
            };
        }
        
        ctx.pop_extend();
        
        if items.is_empty() {
            Ok(Expr::Atom(atom))
        } else {
            Ok(Expr::Primary(Primary::new(atom, items)))
        }
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
                    Atom::Identifier(self.interner.get_or_intern(name.as_str()).into())
                },
                
                // Literals
                Token::Nil   => Atom::Nil,
                Token::True  => Atom::BooleanLiteral(true),
                Token::False => Atom::BooleanLiteral(false),
                
                Token::IntegerLiteral(value) => Atom::IntegerLiteral(value),
                Token::FloatLiteral(value)   => Atom::FloatLiteral(value),
                Token::StringLiteral(value)   => {
                    Atom::StringLiteral(self.interner.get_or_intern(value.as_str()).into())
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
