use crate::parser::expr::{Expr, ExprVariant};
use crate::parser::primary::{Primary, Atom};
use crate::runtime::variant::Variant;
use crate::interpreter::runtime::Scope;
use crate::interpreter::errors::EvalResult;


fn eval(local: &Scope<'_>, expr: &Expr) -> EvalResult<Variant> {
    EvalContext::new(local, expr).eval()
}


// tracks the local scope and the innermost Expr
pub struct EvalContext<'r> {
    local: &'r Scope<'r>,
    expr: &'r Expr,
}

impl<'r> EvalContext<'r> {
    pub fn new(local: &'r Scope, expr: &'r Expr) -> Self {
        EvalContext { local, expr }
    }
    
    pub fn eval(&self) -> EvalResult<Variant> {
        self.eval_inner_expr(self.expr.variant())
    }
    
    fn eval_inner_expr(&self, expr: &ExprVariant) -> EvalResult<Variant> {
        match expr {
            ExprVariant::Primary(primary) => self.eval_primary(primary),
            ExprVariant::UnaryOp(op, expr) => unimplemented!(),
            ExprVariant::BinaryOp(op, lhs, rhs) => unimplemented!(),
            ExprVariant::Assignment(assignment) => unimplemented!(),
            ExprVariant::Tuple(expr_list) => unimplemented!(),
            ExprVariant::ObjectCtor(ctor) => unimplemented!(),
        }
    }

    fn eval_primary(&self, primary: &Primary) -> EvalResult<Variant> {
        let mut value = self.eval_atom(primary.atom())?;
        
        for item in primary.iter_path() {
            unimplemented!();
        }
        
        Ok(value)
    }

    fn eval_atom(&self, atom: &Atom) -> EvalResult<Variant> {
        let value = match atom {
            Atom::Nil => Variant::Nil,
            Atom::EmptyTuple => Variant::EmptyTuple,
            Atom::BooleanLiteral(value) => Variant::Boolean(*value),
            Atom::IntegerLiteral(value) => Variant::Integer(*value),
            Atom::FloatLiteral(value) => Variant::Float(*value),
            Atom::StringLiteral(value) => Variant::InternStr(*value),
            
            Atom::Identifier(name) => unimplemented!(),
            Atom::GlobalIdentifier(name) => unimplemented!(),
            
            Atom::Group(expr) => self.eval_inner_expr(expr)?,
        };
        Ok(value)
    }
}

