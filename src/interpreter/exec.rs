use crate::parser::stmt::{StmtMeta, Stmt};
use crate::runtime::Variant;
use crate::runtime::errors::{ExecResult, RuntimeError};

use crate::interpreter::{ControlFlow, EvalContext, Environment};
use crate::interpreter::eval::{try_value, EvalResult};


pub struct ExecContext<'a, 'r, 's> {
    local_env: &'a Environment<'r, 's>,
}

impl<'a, 'r, 's> ExecContext<'a, 'r, 's> {
    pub fn new(local_env: &'a Environment<'r, 's>) -> Self {
        ExecContext { local_env }
    }
    
    pub fn exec(&self, stmt: &StmtMeta) -> ExecResult<ControlFlow> {
        self.exec_stmt(stmt.variant())
    }
    
    pub fn exec_stmt(&self, stmt: &Stmt) -> ExecResult<ControlFlow> {
        let control = match stmt {
            Stmt::Echo(expr) => {
                let eval_ctx = EvalContext::new(self.local_env);
                let value = try_value!(eval_ctx.eval_expr(&expr)?);
                
                println!("{}", value.repr(self.local_env.string_table()));
                
                ControlFlow::None
            },
            
            Stmt::Expression(expr) => {
                let eval_ctx = EvalContext::new(self.local_env);
                try_value!(eval_ctx.eval_expr(&expr)?);
                ControlFlow::None
            }
            
            Stmt::Continue(label) => ControlFlow::Continue(*label),
            
            Stmt::Break(label, expr) => {
                let eval_ctx = EvalContext::new(self.local_env);
                
                let value = try_value!(expr.as_ref().map_or(
                    Ok(Variant::Nil.into()), 
                    |expr| eval_ctx.eval_expr(&expr)
                )?);
                
                ControlFlow::Break(*label, value)
            },
            
            Stmt::Return(expr) => {
                let eval_ctx = EvalContext::new(self.local_env);
                
                let value = try_value!(expr.as_ref().map_or(
                    Ok(Variant::Nil.into()), 
                    |expr| eval_ctx.eval_expr(&expr)
                )?);
                
                ControlFlow::Return(value)
            },
        };
        
        Ok(control)
    }

}
