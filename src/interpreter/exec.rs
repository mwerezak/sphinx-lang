
use std::io::{self, Write};
use crate::parser::stmt::StmtVariant;
use crate::runtime::{Environment, Variant};
use crate::runtime::errors::{ExecResult, RuntimeError};
use crate::interpreter;


pub struct ExecContext<'a, 'r> {
    env: &'a mut Environment<'r>,
}

impl<'a, 'r> From<&'a mut Environment<'r>> for ExecContext<'a, 'r> {
    fn from(env: &'a mut Environment<'r>) -> Self {
        ExecContext { env }
    }
}

impl<'a, 'r> ExecContext<'a, 'r> {
    pub fn exec(&mut self, stmt: &StmtVariant) -> ExecResult<()> {
        match stmt {
            StmtVariant::Echo(expr) => {
                let value = interpreter::eval(&mut self.env, expr)?;
                
                let mut buf = String::new();
                value.write_repr(&mut buf, self.env.runtime())
                    .map_err(|err| RuntimeError::new(err))?;
                
                println!("{}", buf);
            },
            
            StmtVariant::Expression(expr) => {
                // eval an expression just for side effects
                interpreter::eval(&mut self.env, expr)?;
            }
        }
        
        Ok(())
    }

}
