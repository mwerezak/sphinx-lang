use std::collections::VecDeque;

use log::debug;

use crate::source::ModuleSource;
use crate::lexer::{TokenMeta, Token, TokenIndex, LexerError};
use crate::runtime::strings::{StringSymbol, STRING_TABLE};

use expr::{ExprMeta, Expr};
use stmt::{StmtMeta, Stmt, Label};
use primary::{Primary, Atom, AccessItem};
use assign::{Assignment, LValue, Declaration, DeclType};
use operator::{UnaryOp, BinaryOp, Precedence, PRECEDENCE_START, PRECEDENCE_END};
use fundefs::{FunctionDef, FunSignature, FunParam};
use structs::{ObjectConstructor};
use errors::{ErrorPrototype, ErrorKind, ErrorContext, ContextTag};


mod errors;
mod tests;

pub mod expr;
pub mod stmt;
pub mod primary;
pub mod assign;
pub mod operator;
pub mod fundefs;
pub mod structs;

pub use errors::{ParserError, ContextFrame};


// Recursive descent parser

pub struct Parser<'m, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    module: &'m ModuleSource,
    tokens: T,
    next: Option<Result<TokenMeta, LexerError>>,
    errors: VecDeque<ErrorPrototype>,
}

impl<'m, T> Iterator for Parser<'m, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    type Item = Result<StmtMeta, ParserError<'m>>;
    fn next(&mut self) -> Option<Self::Item> { self.next_stmt() }
}

type InternalResult<T> = Result<T, ErrorPrototype>;

impl<'m, I> Parser<'m, I> where I: Iterator<Item=Result<TokenMeta, LexerError>> {
    
    pub fn new(module: &'m ModuleSource, tokens: I) -> Self {
        Parser {
            module,
            tokens,
            next: None,
            errors: VecDeque::new(),
        }
    }
    
    /// for debugging
    fn current_index(&mut self) -> TokenIndex { self.peek().unwrap().span.index }
    
    fn get_str_symbol(&mut self, string: &str) -> StringSymbol {
        STRING_TABLE.write().unwrap().get_or_intern(string)
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
    
    // peek() will consume any errors it encounters (i.e. peek() acts like advance() if the next token was a lexer error)
    // this is so that we don't have to do a complex map_err() every single time we call self.peek()
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
            Err(error) => {
                self.errors.push_back(error);
                let error = self.errors.pop_front().unwrap();
                let error = Self::process_error(ctx, error);
                
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
    
    fn catch_error_and_sync<T>(&mut self, ctx: &ErrorContext, result: InternalResult<T>, inside_block: bool) -> Option<InternalResult<T>> {
        match result {
            Ok(..) => Some(result),
            Err(error) => {
                if matches!(error.kind(), ErrorKind::EndofTokenStream) {
                    return Some(Err(error));
                }
                
                self.errors.push_back(error.with_symbol_from_ctx(&ctx));
                self.synchronize_stmt(inside_block);
                
                // if the next token is EOF there is no point catching an error
                // since there is no more source code to examine anyways
                // (same applies if we can't even peek without hitting an error)
                match self.peek() {
                    Err(..) | Ok(TokenMeta { token: Token::EOF, .. }) 
                      => Some(Err(self.errors.pop_front().unwrap())),
                    
                    _ => None,
                }
            }
        }
    }
    
    // If we hit an error we need to synchronize back to a likely-valid state before we continue parsing again
    // To do this, just keep discarding tokens until we think we're at the start of a new statement
    fn synchronize_stmt(&mut self, inside_block: bool) {
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
                
                Token::End if inside_block => break,
                
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
        
        let stmt = self.parse_toplevel_stmt(ctx)?;
        let symbol = ctx.frame().as_debug_symbol().unwrap();
        
        ctx.pop_extend();
        Ok(StmtMeta::new(stmt, symbol))
    }
    
    fn parse_toplevel_stmt(&mut self, ctx: &mut ErrorContext) -> InternalResult<Stmt> {
        let stmt = match  self.peek()?.token {
            Token::While => unimplemented!(),
            Token::Do => unimplemented!(),
            Token::For => unimplemented!(),
            Token::Echo => {
                ctx.set_end(&self.advance().unwrap());
                Stmt::Echo(self.parse_expr_variant(ctx)?)
            },
            
            Token::Continue | Token::Break | Token::Return => {
                ctx.push(ContextTag::ControlFlow);
                
                let next = self.advance().unwrap();
                ctx.set_start(&next);
                
                let name = match next.token {
                    Token::Continue => "continue",
                    Token::Break => "break",
                    Token::Return => "return",
                    _ => unreachable!(),
                };
                
                let message = format!("\"{}\" is not allowed here", name);
                return Err(ErrorKind::SyntaxError(message).into());
            },
            
            _ => Stmt::Expression(self.parse_expr_variant(ctx)?),
        };
        Ok(stmt)
    }
    
    fn parse_stmt_list(&mut self, ctx: &mut ErrorContext) -> InternalResult<Vec<StmtMeta>> {
        ctx.push(ContextTag::StmtList);
        
        let mut suite = Vec::new();
        
        debug!("enter stmt list at index {}...", self.current_index());
        
        loop {
            
            let next = self.peek()?;
            if matches!(next.token, Token::End) {
                break;
            }
            
            let parse_result = self.try_parse_control_flow_stmt(ctx);
            let control_stmt = match self.catch_error_and_sync(ctx, parse_result, true) {
                Some(result) => result?,
                None => continue,
            };
            
            if let Some(control) = control_stmt {
                
                let next = self.peek()?;
                if !matches!(next.token, Token::End) {
                    // consume the unexpected token so that it is included in the error message
                    ctx.set_end(&self.advance().unwrap());
                    
                    let name = match control.variant() {
                        Stmt::Continue(..) => "continue",
                        Stmt::Break(..) => "break",
                        Stmt::Return(..) => "return",
                        _ => unreachable!(),
                    };
                    
                    let message = format!("\"{}\" must be the last statement in a block", name);
                    let error = ErrorPrototype::from(ErrorKind::SyntaxError(message.into()))
                        .with_symbol_from_ctx(&ctx);
                    
                    self.errors.push_back(error);
                
                } else{
                    suite.push(control);
                }
                
                break;
            }
            
            let parse_result = self.parse_stmt(ctx);
            let stmt = match self.catch_error_and_sync(ctx, parse_result, true) {
                Some(result) => result?,
                None => continue,
            };
            
            suite.push(stmt);
        }
        
        ctx.set_end(&self.advance().unwrap());
        
        debug!("exit stmt list at {}...", self.current_index());
        
        ctx.pop_extend();
        Ok(suite)
    }
    
    fn try_parse_label(&mut self, _ctx: &mut ErrorContext) -> InternalResult<Option<Label>> {
        let next = self.peek()?;
        
        let label = if let Token::Label(..) = next.token {
            let next = self.advance().unwrap();
            
            if let Token::Label(name) = next.token {
                Some(Label::new(self.get_str_symbol(name.as_str())))
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
        
        let global_token = 
            if let Token::Global = self.peek()?.token { Some(self.advance().unwrap()) }
            else { None };
        
        let expr = self.parse_tuple_expr(ctx)?;
        
        let next = self.peek()?;
        if let Some(op) = Self::which_assignment_op(&next.token) {
            // consume assign_op token
            ctx.push_continuation(ContextTag::AssignmentExpr);
            ctx.set_end(&self.advance().unwrap());
            
            if let Some(ref token) = global_token {
                ctx.set_start(token);
            }
            
            // LHS of assignment has to be an lvalue
            let lhs = LValue::try_from(expr).map_err(|_| ErrorPrototype::from("can't assign to this"))?;
            let rhs = self.parse_expr_variant(ctx)?;
            
            ctx.pop_extend();
            
            let op = op.map(|op| op.into());
            let global = global_token.is_some();
            let assign = Box::new(Assignment { lhs, op, rhs, global });
            return Ok(Expr::Assignment(assign));
            
        } else if global_token.is_some() {
            return Err("expected an assignment expression after \"global\"".into())
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
            _ => panic!("parse_vardecl_expr() called but next token was neither let nor var"),
        };
        
        let expr = self.parse_tuple_expr(ctx)?;
        let lhs = LValue::try_from(expr).map_err(|_| ErrorPrototype::from("can't assign to this"))?;
        if !Self::is_lvalue_valid_for_decl(&lhs) {
            return Err("only identifiers can be used in a variable declaration".into());
        }
        
        // check for and consume "="
        let next = self.advance()?;
        ctx.set_end(&next);
        
        if let Some(op) = Self::which_assignment_op(&next.token) {
            if op.is_some() {
                return Err("update-assignment is not allowed in a variable declaration".into());
            }
        } else {
            return Err("missing \"=\" in variable declaration".into());
        }
        
        let init = self.parse_expr_variant(ctx)?;
        
        ctx.pop_extend();
        
        let decl = Box::new(Declaration { decl, lhs, init });
        return Ok(Expr::Declaration(decl));
    }
    
    fn is_lvalue_valid_for_decl(lvalue: &LValue) -> bool {
        match lvalue {
            LValue::Identifier(..) => true,
            LValue::Tuple(lvalue_list) 
                => lvalue_list.iter().all(|lvalue| Self::is_lvalue_valid_for_decl(lvalue)),
            _ => false,
        }
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
            Ok(Expr::Tuple(tuple_exprs.into_boxed_slice()))
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
            Token::Fun => self.parse_function_def(ctx)?,
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
            return Err("block labels must be followed by either a block or a loop".into());
        }
        
        let mut suite = self.parse_stmt_list(ctx)?;
        
        // SYNTACTIC SUGAR: convert expression statement at end of block into "break <expr>"
        if let Some(Stmt::Expression(..)) = suite.last().map(|stmt| stmt.variant()) {
            let (stmt, symbol) = suite.pop().unwrap().take();
            
            let expr = match stmt {
                Stmt::Expression(expr) => expr,
                _ => unreachable!(),
            };
            
            let break_stmt = StmtMeta::new(Stmt::Break(None, Some(expr)), symbol);
            suite.push(break_stmt);
        }
        
        ctx.pop_extend();
        Ok(Expr::Block(block_label, suite.into_boxed_slice()))
    }
    
    fn parse_function_def(&mut self, ctx: &mut ErrorContext) -> InternalResult<Expr> {
        ctx.push(ContextTag::FunDefExpr);
        
        let next = self.advance()?;
        ctx.set_start(&next);
        debug_assert!(matches!(next.token, Token::Fun));
        
        // if the next token isn't an open paren, it must be a function name
        let next = self.peek()?;
        
        let name = 
            if !matches!(next.token, Token::OpenParen) {
                Some(self.parse_function_assignment_target(ctx)?)
            } else { None };
        
        // expect open paren now
        let next = self.advance().unwrap();
        ctx.set_end(&next);
        
        // function parameter list
        
        if !matches!(next.token, Token::OpenParen) {
            return Err("missing opening \"(\" before parameter list".into());
        }
        
        let signature = self.parse_function_param_list(ctx)?;
        
        let next = self.advance()?;
        if !matches!(next.token, Token::CloseParen) {
            return Err("missing closing \")\" after parameter list".into());
        }
        
        // function body
        
        let body = self.parse_stmt_list(ctx)?;
        
        let function_def = FunctionDef::new(signature, body);
        
        // SYNTACTIC SUGAR: fun name(..) => let name = fun(..)
        if let Some(lvalue) = name {
            let function_decl = Declaration {
                decl: DeclType::Immutable,
                lhs: lvalue,
                init: Expr::FunctionDef(function_def),
            };
            
            Ok(Expr::Declaration(Box::new(function_decl)))
        } else {
            
            Ok(Expr::FunctionDef(function_def))
        }
    }
    
    fn parse_function_param_list(&mut self, ctx: &mut ErrorContext) -> InternalResult<FunSignature> {

        let mut required = Vec::new();
        let mut default = Vec::new();
        let mut variadic = None;

        loop {
            let next = self.peek()?;
            
            if matches!(next.token, Token::CloseParen) {
                break;
            }
            
            ctx.push(ContextTag::FunParam);
            ctx.set_start(&next);
            
            // mutability modifier
            
            let decl = match next.token {
                Token::Var => Some(DeclType::Mutable),
                Token::Let => Some(DeclType::Immutable),
                Token::Identifier(..) => None,
                _ => return Err("invalid parameter".into()),
            };
            
            if decl.is_some() {
                ctx.set_end(&self.advance().unwrap());
            }
            
            let decl = decl.unwrap_or(DeclType::Immutable);
            
            // name
            
            let next = self.advance()?;
            ctx.set_end(&next);
            
            let name = 
                if let Token::Identifier(name) = next.token { name }
                else { return Err("invalid parameter".into()); };
            
            let name = self.get_str_symbol(name.as_str());
            
            // possibly variadic
            
            let next = self.peek()?;
            let is_variadic = matches!(next.token, Token::Ellipsis);
            if is_variadic {
                ctx.set_end(&self.advance().unwrap());
            }
            
            // possible default value
            let next = self.peek()?;
            let default_value =
                if matches!(next.token, Token::OpAssign) {
                    ctx.set_end(&self.advance().unwrap());
                    
                    Some(self.parse_expr_variant(ctx)?)
                } else {
                    None
                };
            
            // expect either a comma "," or the closing ")"
            let next = self.peek()?;
            match next.token {
                // variadic parameter
                Token::Comma if is_variadic => {
                    return Err("a variadic parameter must be the last one in the parameter list".into());
                }
                Token::CloseParen if is_variadic => {
                    variadic.replace(FunParam::new(name, decl, default_value));
                },
                
                // normal parameter
                Token::Comma | Token::CloseParen if !is_variadic => {
                    let has_default = default_value.is_some();
                    
                    let param = FunParam::new(name, decl, default_value);
                    if has_default {
                        default.push(param);
                    } else {
                        if !default.is_empty() {
                            return Err("cannot have a non-default parameter after a default parameter".into());
                        }
                        required.push(param);
                    }
                },
                
                _ => return Err("invalid parameter".into()),
            }
            
            if matches!(next.token, Token::Comma) {
                ctx.set_end(&self.advance().unwrap());
            }
            
            ctx.pop_extend();
        }
        
        Ok(FunSignature::new(required, default, variadic))
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
                Token::OpAccess => items.push(self.parse_member_access(ctx)?),
                
                // subscript ::= "[" expression "]" ;
                Token::OpenSquare => items.push(self.parse_index_access(ctx)?),
                
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
    
    // similar to parse_primary(), except we only allow member access and index access, and convert to an LValue after
    fn parse_function_assignment_target(&mut self, ctx: &mut ErrorContext) -> InternalResult<LValue> {
        ctx.push(ContextTag::PrimaryExpr);
        
        let atom = self.parse_atom(ctx)?;
        
        let mut items = Vec::new();
        loop {
            let next = self.peek()?;
            match next.token {
                
                // access ::= "." IDENTIFIER ;
                Token::OpAccess => items.push(self.parse_member_access(ctx)?),
                
                // subscript ::= "[" expression "]" ;
                Token::OpenSquare => items.push(self.parse_index_access(ctx)?),
                
                _ => break,
            };
        }
        
        ctx.pop_extend();
        
        let lvalue =
            if items.is_empty() { LValue::try_from(atom) } 
            else { LValue::try_from(Primary::new(atom, items)) }
            .map_err(|_| ErrorPrototype::from("cannot assign a function to this"))?;
        
        Ok(lvalue)
    }
    
    // access ::= "." IDENTIFIER ;
    fn parse_member_access(&mut self, ctx: &mut ErrorContext) -> InternalResult<AccessItem> {
        ctx.push(ContextTag::MemberAccess);
        ctx.set_start(&self.advance().unwrap());
        
        let next = self.advance()?;
        ctx.set_end(&next);
        
        let item;
        if let Token::Identifier(name) = next.token {
            item = AccessItem::Attribute(self.get_str_symbol(name.as_str()));
        } else {
            return Err("invalid Identifier".into());
        }
        
        ctx.pop_extend();
        Ok(item)
    }
    
    // subscript ::= "[" expression "]" ;
    fn parse_index_access(&mut self, ctx: &mut ErrorContext) -> InternalResult<AccessItem> {
        ctx.push(ContextTag::IndexAccess);
        ctx.set_start(&self.advance().unwrap());
        
        let index_expr = self.parse_expr(ctx)?;
        
        let next = self.advance()?;
        ctx.set_end(&next);
        
        let item;
        if matches!(next.token, Token::CloseSquare) {
            item = AccessItem::Index(index_expr);
        } else {
            return Err("missing closing \"]\"".into());
        }
        
        ctx.pop_extend();
        Ok(item)
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
                    Atom::Identifier(self.get_str_symbol(name.as_str()))
                },
                
                // Literals
                Token::Nil   => Atom::Nil,
                Token::True  => Atom::BooleanLiteral(true),
                Token::False => Atom::BooleanLiteral(false),
                
                Token::IntegerLiteral(value) => Atom::IntegerLiteral(value),
                Token::FloatLiteral(value)   => Atom::FloatLiteral(value),
                Token::StringLiteral(value)   => {
                    Atom::StringLiteral(self.get_str_symbol(value.as_str()))
                },
                
                _ => { 
                    return Err("expected an expression here".into())
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
            return Err("missing closing \")\"".into());
        }
        
        ctx.pop_extend();
        Ok(Atom::Group(Box::new(expr)))
    }
    
}
