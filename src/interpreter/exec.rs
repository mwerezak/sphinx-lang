
use crate::parser::expr::ExprMeta;
use crate::parser::stmt::{StmtMeta, Stmt};
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
    pub fn exec(&self, stmt: &StmtMeta) -> ExecResult<()> {
        match stmt.variant() {
            Stmt::Echo(expr) => {
                let value = interpreter::eval_expr_variant(&self.local_env, &expr)?;
                
                let mut buf = String::new();
                value.write_repr(&mut buf, self.local_env.string_table())
                    .map_err(|err| RuntimeError::new(err))?;
                
                println!("{}", buf);
            },
            
            Stmt::Expression(expr) => {
                // eval an expression just for side effects
                interpreter::eval_expr_variant(&self.local_env, &expr)?;
            }
            
            Stmt::Continue(_label) => unimplemented!(),
            Stmt::Break(_label, _value) => unimplemented!(),
            Stmt::Return(_value) => unimplemented!(),
        }
        
        Ok(())
    }

}
