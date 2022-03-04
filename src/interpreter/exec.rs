// use std::io::{self, Write};
// use crate::parser::stmt::StmtVariant;
// use crate::runtime::errors::{ExecResult, ExecError, ExecErrorKind as ErrorKind};
// use crate::interpreter::runtime::Environment;
// use crate::interpreter::eval::eval_expr;

// pub fn exec_stmt<'r>(local: &'r mut Environment<'r>, expr: &StmtVariant) -> ExecResult<()> {
//     let mut ctx = ExecContext::from(local);
//     ctx.exec(expr)
// }


// pub struct ExecContext<'r> {
//     env: &'r mut Environment<'r>,
// }

// impl<'r> From<&mut Environment<'r>> for ExecContext<'r> {
//     fn from(env: &mut Environment<'r>) -> ExecContext<'r> {
//         ExecContext { env }
//     }
// }

// impl<'r> ExecContext<'r> {
//     pub fn exec(&'r mut self, stmt: &StmtVariant) -> ExecResult<()> {
//         match stmt {
//             StmtVariant::Echo(expr) => {
                
//             },
            
//             StmtVariant::Expression(expr) => {
//                 // eval an expression just for side effects
//                 eval_expr(&mut self.env, expr)
//                     .map_err(|err| ExecError::from(ErrorKind::FailedEval).caused_by(err) )?;
//             }
//         }
        
//         Ok(())
//     }
// }