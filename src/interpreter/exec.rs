
use crate::parser::expr::Expr;
use crate::parser::stmt::{Stmt, StmtVariant};
use crate::runtime::Environment;
use crate::runtime::errors::{ExecResult, RuntimeError};
use crate::interpreter;


pub struct ExecContext<'a, 'r, 's> {
    local_env: &'a Environment<'r, 's>,
}

impl<'a, 'r, 's> From<&'a Environment<'r, 's>> for ExecContext<'a, 'r, 's> {
    fn from(local_env: &'a Environment<'r, 's>) -> Self {
        ExecContext { local_env }
    }
}

impl ExecContext<'_, '_, '_> {
    pub fn exec(&self, stmt: &Stmt) -> ExecResult<()> {
        match stmt.variant() {
            StmtVariant::Echo(expr) => {
                let value = interpreter::eval_expr_variant(&self.local_env, &expr)?;
                
                let mut buf = String::new();
                value.write_repr(&mut buf, self.local_env.string_table())
                    .map_err(|err| RuntimeError::new(err))?;
                
                println!("{}", buf);
            },
            
            StmtVariant::Expression(expr) => {
                // eval an expression just for side effects
                interpreter::eval_expr_variant(&self.local_env, &expr)?;
            }
            
            StmtVariant::Continue(_label) => unimplemented!(),
            StmtVariant::Break(_label, _value) => unimplemented!(),
            StmtVariant::Return(_value) => unimplemented!(),
        }
        
        Ok(())
    }

}
