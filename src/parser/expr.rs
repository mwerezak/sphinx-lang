use crate::debug::DebugSymbol;
use crate::parser::operator::{BinaryOp, UnaryOp};
use crate::parser::primary::{Atom, Primary};
use crate::parser::lvalue::Assignment;
use crate::parser::fundefs::FunctionDef;
use crate::parser::stmt::{StmtMeta, Stmt, Label, StmtList};

// TODO replace Vecs with boxed slices
#[derive(Debug, Clone)]
pub enum Expr {
    
    Atom(Atom),
    
    Primary(Primary),
    
    UnaryOp(UnaryOp, Box<Expr>),
    
    BinaryOp(BinaryOp, Box<(Expr, Expr)>),
    
    Assignment(Box<Assignment>),
    
    Tuple(Box<[ExprMeta]>),
    
    Unpack(Option<Box<Expr>>),
    
    // ObjectCtor(Box<ObjectConstructor>),
    
    IfExpr {
        branches: Box<[ConditionalBranch]>,
        else_clause: Option<Box<ExprBlock>>,
    },
    
    Block {
        label: Option<Label>, 
        suite: Box<ExprBlock>,
    },
    
    FunctionDef(FunctionDef),
    
    // ClassDef
    
}


/// represents a statement list used as an expression
#[derive(Debug, Clone)]
pub struct ExprBlock {
    stmt_list: StmtList,
    result: Option<ExprMeta>,
}

impl From<StmtList> for ExprBlock {
    fn from(stmt_list: StmtList) -> Self {
        let (mut suite, control) = stmt_list.take();
        
        let mut result = None;
        if control.is_none() && !suite.is_empty() {
            match ExprMeta::try_from(suite.pop().unwrap()) {
                Err(stmt) => suite.push(stmt), // put it back
                Ok(expr) => { result.replace(expr); },
            }
        }
        
        let stmt_list = StmtList::new(suite, control);
        Self { stmt_list, result }
    }
}

impl ExprBlock {
    pub fn stmt_list(&self) -> &StmtList { &self.stmt_list }
    pub fn result(&self) -> Option<&ExprMeta> { self.result.as_ref() }
}



#[derive(Debug, Clone)]
pub struct ConditionalBranch {
    condition: Expr,
    suite: ExprBlock,
}

impl ConditionalBranch {
    pub fn new(condition: Expr, suite: ExprBlock) -> Self {
        Self { condition, suite }
    }
    
    pub fn condition(&self) -> &Expr { &self.condition }
    pub fn suite(&self) -> &ExprBlock { &self.suite }
}


/// An `Expr` plus a `DebugSymbol`
#[derive(Debug, Clone)]
pub struct ExprMeta {
    variant: Expr,
    symbol: DebugSymbol,
}

impl ExprMeta {
    pub fn new(variant: Expr, symbol: DebugSymbol) -> Self {
        ExprMeta { variant, symbol }
    }
    
    pub fn variant(&self) -> &Expr { &self.variant }
    pub fn take_variant(self) -> Expr { self.variant }
    
    pub fn debug_symbol(&self) -> &DebugSymbol { &self.symbol }
    pub fn take_symbol(self) -> DebugSymbol { self.symbol }
    
    pub fn take(self) -> (Expr, DebugSymbol) { (self.variant, self.symbol) }
}

impl From<ExprMeta> for (Expr, DebugSymbol) {
    fn from(expr: ExprMeta) -> Self { (expr.variant, expr.symbol) }
}



// conversion to/from expression-statements

impl From<Expr> for Stmt {
    #[inline]
    fn from(expr: Expr) -> Self { Stmt::Expression(expr) }
}

impl TryFrom<Stmt> for Expr {
    type Error = Stmt;
    
    #[inline]
    fn try_from(stmt: Stmt) -> Result<Self, Stmt> {
        if let Stmt::Expression(expr) = stmt { Ok(expr) }
        else { Err(stmt) }
    }
}

impl TryFrom<StmtMeta> for Expr {
    type Error = StmtMeta;
    
    #[inline]
    fn try_from(stmt: StmtMeta) -> Result<Self, StmtMeta> {
        let (stmt, symbol) = stmt.take();
        if let Stmt::Expression(expr) = stmt { Ok(expr) }
        else { Err(StmtMeta::new(stmt, symbol)) }
    }
}


impl From<ExprMeta> for StmtMeta {
    #[inline]
    fn from(expr: ExprMeta) -> Self {
        let (expr, symbol) = expr.take();
        StmtMeta::new(expr.into(), symbol)
    }
}

impl TryFrom<StmtMeta> for ExprMeta {
    type Error = StmtMeta;
    
    #[inline]
    fn try_from(stmt: StmtMeta) -> Result<Self, StmtMeta> {
        let (stmt, symbol) = stmt.take();
        match Expr::try_from(stmt) {
            Ok(expr) => Ok(ExprMeta::new(expr, symbol)),
            Err(stmt) => Err(StmtMeta::new(stmt, symbol)),
        }
    }
}