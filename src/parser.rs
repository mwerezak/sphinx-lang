use std::collections::VecDeque;

use log::debug;

use crate::lexer::{TokenMeta, Token, TokenIndex, LexerError};
use crate::runtime::strings::{InternSymbol, StringInterner};
use crate::debug::SourceError;


pub mod expr;
pub mod stmt;
pub mod primary;
pub mod lvalue;
pub mod operator;
pub mod fundefs;
pub mod errors;
mod tests;

pub use errors::{ParserError, ParseResult};

use expr::{ExprMeta, Expr, ExprBlock, ConditionalBranch};
use stmt::{StmtMeta, StmtList, Stmt, Label, ControlFlow};
use primary::{Primary, Atom, AccessItem};
use lvalue::{Assignment, LValue, Declaration, DeclType};
use operator::{UnaryOp, BinaryOp, Precedence, PRECEDENCE_START, PRECEDENCE_END};
use fundefs::{FunctionDef, SignatureDef, ParamDef};
use errors::{ErrorKind, ErrorContext, ContextTag};


// Recursive descent parser

pub struct Parser<'h, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    interner: &'h mut StringInterner,
    tokens: T,
    next: Option<Result<TokenMeta, LexerError>>,
    errors: VecDeque<ParserError>,
}

impl<'h, T> Iterator for Parser<'h, T> where T: Iterator<Item=Result<TokenMeta, LexerError>> {
    type Item = Result<StmtMeta, ParserError>;
    fn next(&mut self) -> Option<Self::Item> { self.next_stmt() }
}

impl<'h, I> Parser<'h, I> where I: Iterator<Item=Result<TokenMeta, LexerError>> {
    
    pub fn new(interner: &'h mut StringInterner, tokens: I) -> Self {
        Parser {
            tokens, interner,
            next: None,
            errors: VecDeque::new(),
        }
    }
    
    /// for debugging
    fn current_index(&mut self) -> TokenIndex { self.peek().unwrap().span.index }
    
    fn advance(&mut self) -> ParseResult<TokenMeta> {
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
    fn peek(&mut self) -> ParseResult<&TokenMeta> {
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
    
    fn intern_str(&mut self, string: impl AsRef<str>) -> InternSymbol {
        self.interner.get_or_intern(string)
    }
    
    pub fn next_stmt(&mut self) -> Option<Result<StmtMeta, ParserError>> {
        let mut ctx = ErrorContext::new(ContextTag::TopLevel);
        
        if let Some(error) = self.errors.pop_front() {
            return Some(Err(Self::process_error(ctx, error)));
        }
        
        // check stop conditions
        loop {
            match self.peek() {
                Err(error) if matches!(error.kind(), ErrorKind::EndofTokenStream) => return None,
                
                Err(error) => return Some(Err(Self::process_error(ctx, error))),
                
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
    
    fn process_error(ctx: ErrorContext, error: ParserError) -> ParserError {
        debug!("{:#?}", ctx);
        
        let error = error.with_error_context(ctx);
        
        debug!("parser error: {:?}\ncontext: {:?}\nsymbol: {:?}", 
            error.kind(), error.context(), 
            error.debug_symbol(),
        );
        
        error
    }
    
    fn catch_error_and_sync<T>(&mut self, ctx: &ErrorContext, result: ParseResult<T>, inside_block: bool) -> Option<ParseResult<T>> {
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
        
        let mut ctx = ErrorContext::new(ContextTag::Sync);
        
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
    
    fn parse_stmt(&mut self, ctx: &mut ErrorContext) -> ParseResult<StmtMeta> {
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
    
    fn parse_stmt_variant(&mut self, ctx: &mut ErrorContext) -> ParseResult<Stmt> {
        let stmt = match  self.peek()?.token {
            
            Token::Loop => self.parse_loop(ctx, None)?,
            Token::While => self.parse_while_loop(ctx, None)?,
            Token::For => unimplemented!(),
            
            Token::Label(..) => self.parse_stmt_label(ctx)?,
            
            Token::Echo => {
                ctx.set_start(&self.advance().unwrap());
                Stmt::Echo(self.parse_expr_variant(ctx)?)
            },
            Token::Assert => {
                ctx.set_start(&self.advance().unwrap());
                Stmt::Assert(self.parse_expr_variant(ctx)?)
            }
            
            Token::Continue | Token::Break | Token::Return => {
                let next = self.advance().unwrap();
                
                ctx.push(ContextTag::ControlFlow);
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
    
    fn parse_stmt_label(&mut self, ctx: &mut ErrorContext) -> ParseResult<Stmt> {
        let label = self.try_parse_label(ctx)?.unwrap();
        
        match self.peek()?.token {
            Token::Loop => self.parse_loop(ctx, Some(label)),
            Token::While => self.parse_while_loop(ctx, Some(label)),
            Token::For => unimplemented!(),
            
            _ => return Err("labels must be followed by either a block or a loop".into()),
        }
    }
    
    fn parse_loop(&mut self, ctx: &mut ErrorContext, label: Option<Label>) -> ParseResult<Stmt> {
        let next = self.advance()?;
        
        ctx.push(ContextTag::Loop);
        ctx.set_start(&next);
        debug_assert!(matches!(next.token, Token::Loop));
        
        let body = self.parse_stmt_list(ctx, |token| matches!(token, Token::End))?;
        
        let next = self.advance().unwrap(); 
        ctx.set_end(&next);
        
        ctx.pop_extend();
        Ok(Stmt::Loop { label, body })
    }
    
    fn parse_while_loop(&mut self, ctx: &mut ErrorContext, label: Option<Label>) -> ParseResult<Stmt> {
        let next = self.advance()?;
        
        ctx.push(ContextTag::WhileLoop);
        ctx.set_start(&next);
        debug_assert!(matches!(next.token, Token::While));
        
        let condition = self.parse_expr_variant(ctx)?;
        
        let next = self.advance()?;
        ctx.set_end(&next);
        
        if !matches!(next.token, Token::Do) {
            return Err("missing \"do\" after condition in while-loop".into());
        }
        
        let body = self.parse_stmt_list(ctx, |token| matches!(token, Token::End))?;
        
        ctx.set_end(&self.advance().unwrap()); // consume "end"
        
        ctx.pop_extend();
        Ok(Stmt::WhileLoop { label, condition, body })
    }
    
    /// Parses a list of statements, stopping when the given closure returns true. The final token is not consumed.
    fn parse_stmt_list(&mut self, ctx: &mut ErrorContext, end_list: impl Fn(&Token) -> bool) -> ParseResult<StmtList> {
        ctx.push(ContextTag::StmtList);
        
        let mut suite = Vec::new();
        let mut control = None;
        
        debug!("enter stmt list at index {}...", self.current_index());
        
        loop {
            
            // statement separators
            while matches!(self.peek()?.token, Token::Semicolon) {
                ctx.set_end(&self.advance().unwrap());
            }
            
            let next = self.peek()?;
            if end_list(&next.token) {
                break;
            }
            
            let parse_result = self.try_parse_control_flow(ctx);
            control = match self.catch_error_and_sync(ctx, parse_result, true) {
                Some(result) => result?,
                None => continue,
            };
            
            // if we found a control flow statement, it must be the last item in the statement list
            if let Some(control) = control.as_ref() {
                
                let next = self.peek()?;
                if !end_list(&next.token) {
                    // consume the unexpected token so that it is included in the error message
                    ctx.set_end(&self.advance().unwrap());
                    
                    let name = match control {
                        ControlFlow::Continue(..) => "continue",
                        ControlFlow::Break(..) => "break",
                        ControlFlow::Return(..) => "return",
                    };
                    
                    let message = format!("\"{}\" must be the last statement in a block", name);
                    let error = ParserError::from(ErrorKind::SyntaxError(message.into()))
                        .with_symbol_from_ctx(&ctx);
                    
                    self.errors.push_back(error);
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
        
        debug!("exit stmt list at {}...", self.current_index());
        
        ctx.pop_extend();
        
        Ok(StmtList::new(suite, control))
    }
    
    fn try_parse_label(&mut self, _ctx: &mut ErrorContext) -> ParseResult<Option<Label>> {
        let next = self.peek()?;
        
        let label = if let Token::Label(..) = next.token {
            let next = self.advance().unwrap();
            
            if let Token::Label(name) = next.token {
                Some(Label::new(self.intern_str(name)))
            } else { unreachable!() }
            
        } else { None };
        Ok(label)
    }
    
    fn try_parse_control_flow(&mut self, ctx: &mut ErrorContext) -> ParseResult<Option<ControlFlow>> {
        let next = self.peek()?;
        
        let control_flow = match next.token {
            Token::Continue => {
                ctx.push(ContextTag::ControlFlow);
                ctx.set_start(&self.advance().unwrap());
                
                let label = self.try_parse_label(ctx)?;
                
                ControlFlow::Continue(label)
            },
            
            Token::Break => {
                ctx.push(ContextTag::ControlFlow);
                ctx.set_start(&self.advance().unwrap());
                
                let label = self.try_parse_label(ctx)?;
                
                let expr = 
                    if !matches!(self.peek()?.token, Token::End | Token::Semicolon) {
                        Some(Box::new(self.parse_expr_variant(ctx)?))
                    } else { None };
                
                ControlFlow::Break(label, expr)
            },
            
            Token::Return => {
                ctx.push(ContextTag::ControlFlow);
                ctx.set_start(&self.advance().unwrap());
                
                let expr = 
                    if !matches!(self.peek()?.token, Token::End | Token::Semicolon) {
                        Some(Box::new(self.parse_expr_variant(ctx)?))
                    } else { None };
                
                ControlFlow::Return(expr)
            }
            
            _ => return Ok(None),
        };
        
        ctx.pop_extend();
        Ok(Some(control_flow))
    }
    
    /*** Expression Parsing ***/
    
    fn parse_expr(&mut self, ctx: &mut ErrorContext) -> ParseResult<ExprMeta> {
        ctx.push(ContextTag::ExprMeta);
        
        let variant = self.parse_expr_variant(ctx)?;
        let symbol = ctx.frame().as_debug_symbol().unwrap();
        
        ctx.pop_extend();
        Ok(ExprMeta::new(variant, symbol))
    }
    
    // the top of the recursive descent stack for expressions
    fn parse_expr_variant(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> {
        match self.peek()?.token {
            Token::Var | Token::Let => self.parse_declaration_expr(ctx),
            _ => self.parse_assignment_expr(ctx)
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
    
    fn parse_assignment_expr(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> {
        
        let optional_token = 
            if let Token::NonLocal = self.peek()?.token { Some(self.advance().unwrap()) }
            else { None };
        
        let expr = self.parse_tuple_expr(ctx)?;
        
        let next = self.peek()?;
        if let Some(op) = Self::which_assignment_op(&next.token) {
            // consume assign_op token
            ctx.push_continuation(ContextTag::AssignmentExpr, None);
            ctx.set_end(&self.advance().unwrap());
            
            if let Some(ref token) = optional_token {
                ctx.set_start(token);
            }
            
            // LHS of assignment has to be an lvalue
            let lhs = LValue::try_from(expr).map_err(|_| ParserError::from("can't assign to this"))?;
            
            // update-assignment not allowed with tuples
            if op.is_some() && matches!(lhs, LValue::Tuple(..)) {
                return Err("can't combine tuple-assignment with update-assignment".into());
            }
            
            let rhs = self.parse_expr_variant(ctx)?;
            
            ctx.pop_extend();
            
            let op = op.map(|op| op.into());
            
            let nonlocal = match optional_token.map(|tok| tok.token) {
                Some(Token::NonLocal) => true,
                _ => false,
            };
            
            let assign = Box::new(Assignment { lhs, op, rhs, nonlocal });
            return Ok(Expr::Assignment(assign));
            
        } else if optional_token.is_some() {
            return Err("expected an assignment expression after \"nonlocal\"".into())
        }
        
        Ok(expr)
    }
    
    /*
        declaration_expression ::= ( "let" | "var" ) lvalue_expr_annotated "=" expression ;
    */
    fn parse_declaration_expr(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> {
        let next = self.advance()?;
        
        ctx.push(ContextTag::DeclarationExpr);
        ctx.set_start(&next);
        
        let decl = match next.token {
            Token::Let => DeclType::Immutable,
            Token::Var => DeclType::Mutable,
            _ => panic!("invalid decl token"),
        };
        
        let expr = self.parse_tuple_expr(ctx)?;
        let lhs = LValue::try_from(expr).map_err(|_| ParserError::from("can't assign to this"))?;
        if !Self::is_lvalue_valid_for_decl(&lhs) {
            return Err("only names can be declared as variables".into());
        }
        
        // check for and consume "="
        let next = self.advance()?;
        ctx.set_end(&next);
        
        match Self::which_assignment_op(&next.token) {
            None => return Err("missing \"=\" in variable declaration".into()),
            Some(op) => if op.is_some() {
                return Err("update-assignment is not allowed in a variable declaration".into());
            }
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
    
    fn parse_tuple_expr(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> {
        
        // if this inner expression ends up being captured as the first
        // element of a tuple, we will want to get its debug symbol.
        ctx.push(ContextTag::ExprMeta);
        
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
                // retroactivly get debug symbol
                let frame = ctx.pop();
                let symbol = frame.as_debug_symbol().unwrap();
                tuple_exprs.push(ExprMeta::new(first_expr, symbol));
                
                ctx.push_continuation(ContextTag::TupleCtor, Some(frame)); // enter the tuple context
            }
            
            ctx.set_end(&self.advance().unwrap()); // consume comma
            
            ctx.push(ContextTag::ExprMeta);
            let next_expr = self.parse_binop_expr(ctx)?;
            let symbol = ctx.frame().as_debug_symbol().unwrap();
            ctx.pop_extend();
            
            tuple_exprs.push(ExprMeta::new(next_expr, symbol));
        }
        
        ctx.pop_extend();
        
        if let Some(expr) = first_expr {
            Ok(expr)
        } else {
            Ok(Expr::Tuple(tuple_exprs.into_boxed_slice()))
        }
    }
    
    /*
        Binary operator syntax:
        
        operand[1] ::= unary ;
        operand[N] ::= operand[N-1] ( OPERATOR[N] operand[N-1] )* ;
    */
    fn parse_binop_expr(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> {
        self.parse_binop_expr_levels(ctx, PRECEDENCE_START)
    }
    
    fn parse_binop_expr_levels(&mut self, ctx: &mut ErrorContext, level: Precedence) -> ParseResult<Expr> {
        if level == PRECEDENCE_END {
            return self.parse_unary_expr(ctx);  // exit binop precedence recursion
        }
        
        let mut expr = self.parse_binop_expr_levels(ctx, level - 1)?;
        
        let mut push_ctx = false;
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
            
            push_ctx = true;
            ctx.push_continuation(ContextTag::BinaryOpExpr, None);
            ctx.set_end(&self.advance().unwrap()); // consume binary_op token
            
            let rhs_expr = self.parse_binop_expr_levels(ctx, level - 1)?;
            
            expr = Expr::BinaryOp(binary_op.into(), Box::new((expr, rhs_expr)));
        }
        
        if push_ctx {
            ctx.pop_extend();
        }
        
        Ok(expr)
    }
    
    /*
        Unary operator syntax:
        
        unary-expression ::= ( "-" | "+" | "not" ) unary | primary ;
    */
    fn parse_unary_expr(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> {
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
    fn parse_primary_expr(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> {
        let expr = match self.peek()?.token {
            Token::Class => unimplemented!(),
            Token::Fun => self.parse_function_decl_expr(ctx)?,
            
            Token::If => self.parse_if_expr(ctx)?,
            Token::Begin => self.parse_block_expr(ctx, None)?,
            
            Token::Label(..) => self.parse_expr_label(ctx)?,
            
            // Token::OpenBrace => Ok(ExprMeta::ObjectCtor(self.parse_object_constructor(ctx)?)),
            
            _ => self.parse_primary(ctx)?,
        };
        Ok(expr)
    }
    
    fn parse_expr_label(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> {
        let label = self.try_parse_label(ctx)?.unwrap();
        
        let expr = match self.peek()?.token {
            Token::Begin => self.parse_block_expr(ctx, Some(label))?,
            _ => return Err("labels must be followed by either a block or a loop".into()),
        };
        
        Ok(expr)
    }
    
    /*
        block-expression ::= ( label )? "begin" ( statement | control-flow | "break" ( label )? expression )* "end" ;  (* break can be supplied a value inside of begin-blocks *)
    */
    fn parse_block_expr(&mut self, ctx: &mut ErrorContext, label: Option<Label>) -> ParseResult<Expr> {
        let next = self.advance()?;
        
        // consume "begin"
        ctx.push(ContextTag::BlockExpr);
        ctx.set_start(&next);
        debug_assert!(matches!(next.token, Token::Begin));
        
        let suite = self.parse_stmt_list(ctx, |token| matches!(token, Token::End))?;
        ctx.set_end(&self.advance().unwrap()); // consume "end"
        
        ctx.pop_extend();
        
        Ok(Expr::Block { label, suite: Box::new(ExprBlock::from(suite)), })
    }
    
    fn parse_if_expr(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> {
        let next = self.advance()?;
        
        ctx.push(ContextTag::IfExpr);
        ctx.set_start(&next);
        
        debug_assert!(matches!(next.token, Token::If));
        
        let mut branches = Vec::new();
        let mut else_clause = None;
        
        loop {
            
            // parse condition
            let cond_expr = self.parse_expr_variant(ctx)?;
            
            let next = self.advance()?;
            ctx.set_end(&next);
            
            if !matches!(next.token, Token::Then) {
                return Err("missing \"then\" after condition in if-expression".into());
            }
            
            let stmt_list = self.parse_stmt_list(ctx, |token| matches!(token, Token::Elif | Token::Else | Token::End))?;
            
            branches.push(ConditionalBranch::new(cond_expr, stmt_list.into()));
            
            let next = self.advance().unwrap();
            ctx.set_end(&next);
            
            match next.token {
                Token::End => break,
                Token::Else => {
                    
                    let stmt_list = self.parse_stmt_list(ctx, |token| matches!(token, Token::End))?;
                    else_clause.replace(ExprBlock::from(stmt_list));
                    
                    ctx.set_end(&self.advance().unwrap()); // consume "end"
                    
                    break;
                },
                
                _ => { }
            }
            
        }
        
        ctx.pop_extend();
        
        let if_expr = Expr::IfExpr { 
            branches: branches.into_boxed_slice(),
            else_clause: else_clause.map(|expr| Box::new(expr)),
        };
        Ok(if_expr)
    }
    
    fn parse_function_decl_expr(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> {
        let next = self.advance()?;
        
        ctx.push(ContextTag::FunDefExpr);
        ctx.set_start(&next);
        debug_assert!(matches!(next.token, Token::Fun));
        
        // if the next token isn't an open paren, it must be a function name
        let next = self.peek()?;
        let name_lvalue =
            if !matches!(next.token, Token::OpenParen) {
                Some(self.parse_function_assignment_target(ctx)?)
            } else { None };
        
        
        let function_def = self.parse_function_def(ctx)?;
        
        // SYNTACTIC SUGAR: fun name(..) => let name = fun(..)
        if let Some(lvalue) = name_lvalue {
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
    
    // similar to parse_primary(), except we only allow member access and index access, and convert to an LValue after
    fn parse_function_assignment_target(&mut self, ctx: &mut ErrorContext) -> ParseResult<LValue> {
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
            .map_err(|_| ParserError::from("cannot assign a function to this"))?;
        
        Ok(lvalue)
    }
    
    fn parse_function_def(&mut self, ctx: &mut ErrorContext) -> ParseResult<FunctionDef> {
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
        
        let body = self.parse_stmt_list(ctx, |token| matches!(token, Token::End))?;
        ctx.set_end(&self.advance().unwrap()); // consume "end"
        
        Ok(FunctionDef::new(signature, body))
    }
    
    fn parse_function_param_list(&mut self, ctx: &mut ErrorContext) -> ParseResult<SignatureDef> {

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
            
            let name = self.intern_str(name);
            
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
                    variadic.replace(ParamDef::new(name, decl, default_value));
                },
                
                // normal parameter
                Token::Comma | Token::CloseParen if !is_variadic => {
                    let has_default = default_value.is_some();
                    
                    let param = ParamDef::new(name, decl, default_value);
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
        
        Ok(SignatureDef::new(required, default, variadic))
    }
    
    /*
        Object Constructor syntax:
        
        object-constructor ::= "{" member-initializer ( "," member-initializer )* "}" ;
        member-initializer ::= ( IDENTIFIER | "[" primary "]" ) ":" expression ;
    
    */
    /*
    fn parse_object_constructor(&mut self, ctx: &mut ErrorContext) -> ParseResult<ObjectConstructor> {
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
    */
    
    /*
        Primary expression syntax:
        
        primary ::= atom ( access | subscript | invocation | object-constructor )* ;
        subscript ::= "[" expression "]" ;
        access ::= "." IDENTIFIER ;
        invocation ::= "(" ... ")" ;  (* WIP *)
        object-constructor ::= "{" member-initializer ( "," member-initializer )* "}" ;
    */
    fn parse_primary(&mut self, ctx: &mut ErrorContext) -> ParseResult<Expr> { 
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
    
    // access ::= "." IDENTIFIER ;
    fn parse_member_access(&mut self, ctx: &mut ErrorContext) -> ParseResult<AccessItem> {
        let next = self.advance().unwrap();
        
        ctx.push(ContextTag::MemberAccess);
        ctx.set_start(&next);
        debug_assert!(matches!(next.token, Token::OpAccess));
        
        let next = self.advance()?;
        ctx.set_end(&next);
        
        let item;
        if let Token::Identifier(name) = next.token {
            item = AccessItem::Attribute(self.intern_str(name));
        } else {
            return Err("invalid Identifier".into());
        }
        
        ctx.pop_extend();
        Ok(item)
    }
    
    // subscript ::= "[" expression "]" ;
    fn parse_index_access(&mut self, ctx: &mut ErrorContext) -> ParseResult<AccessItem> {
        let next = self.advance().unwrap();
        
        ctx.push(ContextTag::IndexAccess);
        ctx.set_start(&next);
        debug_assert!(matches!(next.token, Token::OpenSquare));
        
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
    fn parse_atom(&mut self, ctx: &mut ErrorContext) -> ParseResult<Atom> { 
        
        if let Token::OpenParen = self.peek()?.token {
            Ok(self.parse_group_expr(ctx)?)  // Groups
            
        } else { 
            ctx.push(ContextTag::Atom);
            
            let next = self.advance().unwrap();
            ctx.set_start(&next);
            
            let atom = match next.token {
                // Identifiers
                Token::Identifier(name) => {
                    Atom::Identifier(self.intern_str(name))
                },
                
                // Literals
                Token::Nil   => Atom::Nil,
                Token::True  => Atom::BooleanLiteral(true),
                Token::False => Atom::BooleanLiteral(false),
                
                Token::IntegerLiteral(value) => Atom::IntegerLiteral(value),
                Token::FloatLiteral(value)   => Atom::FloatLiteral(value),
                Token::StringLiteral(value)   => {
                    Atom::StringLiteral(self.intern_str(value))
                },
                
                
                // Error productions
                Token::Class | Token::Fun | Token::If | Token::Var | Token::Let | Token::Begin | Token::Label(..) => {
                    let name = match next.token {
                        Token::Class => "class definitions",
                        Token::Fun => "function definitions",
                        Token::Let | Token::Var => "variable declarations",
                        Token::Begin => "block expressions",
                        _ => "this expression",
                    };
                    let message = format!("{} must be enclosed in parentheses to be used here", name);
                    
                    return Err(ErrorKind::SyntaxError(message).into())
                },
                
                _ => { return Err("expected an expression here".into()) },
            };
            
            ctx.pop_extend();
            Ok(atom)
        }
    }
    
    fn parse_group_expr(&mut self, ctx: &mut ErrorContext) -> ParseResult<Atom> {
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
