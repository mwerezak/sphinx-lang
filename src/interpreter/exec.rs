
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
            },
            
            StmtVariant::Expression(expr) => {
                // eval an expression just for side effects
                interpreter::eval(&mut self.env, expr)
                    .map_err(|err| RuntimeError::from(err))?;
            }
        }
        
        Ok(())
    }
    
    // Placeholder until a better system exists
    fn echo_value(&self, value: &Variant) {
        match value {
            Variant::Nil => println!("nil"),
            Variant::EmptyTuple => println!("()"),
            Variant::BoolTrue => println!("true"),
            Variant::BoolFalse => println!("false"),
            Variant::Integer(value) => println!("{}", *value),
            Variant::Float(value) => println!("{}", *value),
            Variant::InternStr(sym) => {
                let s = self.env.runtime().resolve_str(sym);
                println!("\"{}\"", s);
            },
        }
    }
}
