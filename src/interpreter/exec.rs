
use crate::parser::stmt::StmtVariant;
use crate::runtime::Runtime;
use crate::runtime::errors::{ExecResult, RuntimeError};
use crate::interpreter;


pub struct ExecContext<'a, 'r> {
    runtime: &'a mut Runtime<'r>,
}

impl<'a, 'r> From<&'a mut Runtime<'r>> for ExecContext<'a, 'r> {
    fn from(runtime: &'a mut Runtime<'r>) -> Self {
        ExecContext { runtime }
    }
}

impl<'a, 'r> ExecContext<'a, 'r> {
    pub fn exec(&mut self, stmt: &StmtVariant) -> ExecResult<()> {
        match stmt {
            StmtVariant::Echo(expr) => {
                let value = interpreter::eval(&mut self.runtime, expr)?;
                
                let mut buf = String::new();
                value.write_repr(&mut buf, self.runtime)
                    .map_err(|err| RuntimeError::new(err))?;
                
                println!("{}", buf);
            },
            
            StmtVariant::Expression(expr) => {
                // eval an expression just for side effects
                interpreter::eval(&mut self.runtime, expr)?;
            }
        }
        
        Ok(())
    }

}
