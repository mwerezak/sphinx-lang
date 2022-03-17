use crate::debug::DebugSymbol;
use crate::runtime::types::operator::{BinaryOp, UnaryOp};
use crate::parser::primary::{Atom, Primary};
use crate::parser::assign::{Assignment, Declaration};
use crate::parser::fundefs::FunctionDef;
use crate::parser::structs::{ObjectConstructor};
use crate::parser::stmt::{StmtMeta, Stmt, Label};

// TODO replace Vecs with boxed slices
#[derive(Debug, Clone)]
pub enum Expr {
    
    Atom(Atom),
    
    Primary(Primary),
    
    UnaryOp(UnaryOp, Box<Expr>),
    
    BinaryOp(BinaryOp, Box<(Expr, Expr)>),
    
    Assignment(Box<Assignment>), // box the whole Assignment (instead of just lhs Expr) to keep size of ExprMeta down
    
    Declaration(Box<Declaration>),
    
    Tuple(Box<[ExprMeta]>),
    
    // ObjectCtor(Box<ObjectConstructor>),
    
    IfExpr(Conditional),
    
    Block(Option<Label>, Box<[StmtMeta]>),
    
    FunctionDef(FunctionDef),
    
    // ClassDef
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub branches: Box<[CondBranch]>,
    pub else_branch: Option<Box<[StmtMeta]>>,
}

#[derive(Debug, Clone)]
pub struct CondBranch {
    pub cond: Expr,
    pub suite: Box<[StmtMeta]>,
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