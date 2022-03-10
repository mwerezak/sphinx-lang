use crate::parser::stmt::{StmtMeta, Stmt};
use crate::runtime::{Environment, Variant};
use crate::runtime::errors::{ExecResult, RuntimeError};

use crate::interpreter::{ControlFlow, EvalContext};
use crate::interpreter::eval::{try_value, EvalResult};


pub struct ExecContext<'a, 'r, 's> {
    local_env: &'a Environment<'r, 's>,
}

impl<'a, 'r, 's> ExecContext<'a, 'r, 's> {
    pub fn new(local_env: &'a Environment<'r, 's>) -> Self {
        ExecContext { local_env }
    }
    
    pub fn exec(&self, stmt: &StmtMeta) -> ExecResult<ControlFlow> {
        let control = match stmt.variant() {
            Stmt::Echo(expr) => {
                let eval_ctx = EvalContext::new(self.local_env);
                let value = try_value!(eval_ctx.eval_variant(&expr)?);
                
                let mut buf = String::new();
                value.write_repr(&mut buf, self.local_env.string_table())
                    .map_err(|err| RuntimeError::new(err))?;
                
                println!("{}", buf);
                
                ControlFlow::None
            },
            
            Stmt::Expression(expr) => {
                let eval_ctx = EvalContext::new(self.local_env);
                try_value!(eval_ctx.eval_variant(&expr)?);
                ControlFlow::None
            }
            
            Stmt::Continue(label) => ControlFlow::Continue(*label),
            
            Stmt::Break(label, expr) => {
                let eval_ctx = EvalContext::new(self.local_env);
                
                let value = try_value!(expr.as_ref().map_or(
                    Ok(Variant::Nil.into()), 
                    |expr| eval_ctx.eval_variant(&expr)
                )?);
                
                ControlFlow::Break(*label, value)
            },
            
            Stmt::Return(expr) => {
                let eval_ctx = EvalContext::new(self.local_env);
                
                let value = try_value!(expr.as_ref().map_or(
                    Ok(Variant::Nil.into()), 
                    |expr| eval_ctx.eval_variant(&expr)
                )?);
                
                ControlFlow::Return(value)
            },
        };
        
        Ok(control)
    }

}
