#![allow(unused_variables)]
#![allow(unused_mut)]

use crate::parser::expr::ExprVariant;
use crate::parser::primary::{Primary, Atom};
use crate::parser::assign::{Declaration, Assignment, LValue};

use crate::runtime::{Runtime, Variant};
use crate::runtime::strings::StringValue;
use crate::runtime::ops::*;
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::errors::{EvalResult, EvalErrorKind as ErrorKind};


// tracks the local scope and the innermost Expr
pub struct EvalContext<'a, 'r> {
    // very important to keep 'a and 'r separate
    // otherwise EvalContext would be forced to live as long as 'r!
    runtime: &'a mut Runtime<'r>,
}

impl<'a, 'r> From<&'a mut Runtime<'r>> for EvalContext<'a, 'r> {
    fn from(runtime: &'a mut Runtime<'r>) -> Self {
        EvalContext { runtime }
    }
}

impl<'a, 'r> EvalContext<'a, 'r> {
    pub fn eval(&mut self, expr: &ExprVariant) -> EvalResult<Variant> {
        self.eval_inner_expr(expr)
    }
    
    fn lookup_value(&self, name: StringValue) -> EvalResult<Variant> {
        self.runtime.lookup_value(name)
            .ok_or_else(|| {
                let mut string = String::new();
                name.write_str(&mut string, self.runtime.string_table()).unwrap();
                ErrorKind::NameNotDefined(string).into()
            })
    }
    
    fn eval_inner_expr(&mut self, expr: &ExprVariant) -> EvalResult<Variant> {
        match expr {
            ExprVariant::Primary(primary) => self.eval_primary(primary),
            
            ExprVariant::UnaryOp(op, expr) => self.eval_unary_op((*op).into(), expr),
            ExprVariant::BinaryOp(op, lhs, rhs) => self.eval_binary_op((*op).into(), lhs, rhs),
            
            ExprVariant::Assignment(assignment) => self.eval_assignment(assignment),
            ExprVariant::Declaration(declaration) => self.eval_declaration(declaration),
            
            ExprVariant::Tuple(expr_list) => unimplemented!(),
            ExprVariant::ObjectCtor(ctor) => unimplemented!(),
        }
    }

    fn eval_primary(&mut self, primary: &Primary) -> EvalResult<Variant> {
        let mut value = self.eval_atom(primary.atom())?;
        
        for item in primary.iter_path() {
            unimplemented!();
        }
        
        Ok(value)
    }

    fn eval_atom(&mut self, atom: &Atom) -> EvalResult<Variant> {
        let value = match atom {
            Atom::Nil => Variant::Nil,
            Atom::EmptyTuple => Variant::EmptyTuple,
            Atom::BooleanLiteral(true) => Variant::BoolTrue,
            Atom::BooleanLiteral(false) => Variant::BoolFalse,
            Atom::IntegerLiteral(value) => Variant::Integer(*value),
            Atom::FloatLiteral(value) => Variant::Float(*value),
            Atom::StringLiteral(sym) => Variant::String((*sym).into()),
            
            Atom::Identifier(name) => self.lookup_value(StringValue::from(*name))?,
            
            Atom::Self_ => unimplemented!(),
            Atom::Super => unimplemented!(),
            
            Atom::Group(expr) => self.eval_inner_expr(expr)?,
        };
        Ok(value)
    }
    
    fn eval_short_circuit_logic(&mut self, op: Logical, lhs: &ExprVariant, rhs: &ExprVariant) -> EvalResult<Variant> {
        let lhs_value = self.eval_inner_expr(lhs)?;
        
        let cond = match op {
            Logical::And => !lhs_value.truth_value(),
            Logical::Or => lhs_value.truth_value(),
        };
        
        if cond {
            Ok(lhs_value)
        } else {
            self.eval_inner_expr(rhs)
        }
    }
    
    fn eval_unary_op(&mut self, op: UnaryOp, expr: &ExprVariant) -> EvalResult<Variant> {
        let operand = self.eval_inner_expr(expr)?;
        
        match op {
            UnaryOp::Neg => eval_neg(&operand),
            UnaryOp::Pos => eval_pos(&operand),
            UnaryOp::Inv => eval_inv(&operand),
            UnaryOp::Not => eval_not(&operand),
        }
    }
    
    fn eval_binary_op(&mut self, op: BinaryOp, lhs: &ExprVariant, rhs: &ExprVariant) -> EvalResult<Variant> {
        if let BinaryOp::Logical(logic) = op {
            return self.eval_short_circuit_logic(logic, lhs, rhs);
        }
        
        let lhs_value = self.eval_inner_expr(lhs)?;
        let rhs_value = self.eval_inner_expr(rhs)?;
        
        self.eval_binary_op_values(op, &lhs_value, &rhs_value)
    }
    
    fn eval_binary_op_values(&mut self, op: BinaryOp, lhs: &Variant, rhs: &Variant) -> EvalResult<Variant> {
        let result = match op {
            BinaryOp::Arithmetic(op) => match op {
                Arithmetic::Mul    => eval_mul(&lhs, &rhs)?,
                Arithmetic::Div    => eval_div(&lhs, &rhs)?,
                Arithmetic::Mod    => eval_mod(&lhs, &rhs)?,
                Arithmetic::Add    => eval_add(&lhs, &rhs)?,
                Arithmetic::Sub    => eval_sub(&lhs, &rhs)?,
            },
            
            BinaryOp::Bitwise(op) => match op {
                Bitwise::And => eval_and(&lhs, &rhs),
                Bitwise::Xor => eval_xor(&lhs, &rhs),
                Bitwise::Or  => eval_or(&lhs, &rhs),
            },
            
            BinaryOp::Shift(op) => match op {
                Shift::Left => eval_shl(&lhs, &rhs)?,
                Shift::Right => eval_shr(&lhs, &rhs)?,
            }
            
            BinaryOp::Comparison(op) => match op {
                Comparison::LT     => eval_lt(&lhs, &rhs),
                Comparison::GT     => eval_gt(&lhs, &rhs),
                Comparison::LE     => eval_le(&lhs, &rhs),
                Comparison::GE     => eval_ge(&lhs, &rhs),
                
                Comparison::EQ     => Some(eval_eq(&lhs, &rhs)),
                Comparison::NE     => Some(eval_ne(&lhs, &rhs)),
            
            }.map(Variant::from),
            
            _ => None,
        };
        
        if let Some(value) = result {
            Ok(value)
        } else {
            Self::eval_binary_from_type(op, &lhs, &rhs)
        }
    }
    
    // This will probably be replaced once type system is implemented
    fn eval_binary_from_type(_op: BinaryOp, _lhs: &Variant, _rhs: &Variant) -> EvalResult<Variant> {
        // TODO defer to lhs's type metamethods, or rhs's type reflected metamethod
        unimplemented!()
    }
    
    
    fn eval_assignment(&mut self, assignment: &Assignment) -> EvalResult<Variant> {
        if let LValue::Identifier(name) = assignment.lhs {
            let name = StringValue::from(name);
            let lhs_value = self.lookup_value(name)?;
            let mut rhs_value = self.eval_inner_expr(&assignment.rhs)?;
            
            if let Some(op) = assignment.op {
                rhs_value = self.eval_binary_op_values(op, &lhs_value, &rhs_value)?;
            }
            
            let store_env = self.runtime.lookup_env_mut(name).unwrap();
            store_env.store_value(name, rhs_value);
            Ok(rhs_value)
        } else {
            unimplemented!()
        }
    }
    
    fn eval_declaration(&mut self, declaration: &Declaration) -> EvalResult<Variant> {
        
        if let LValue::Identifier(name) = declaration.lhs {
            let name = StringValue::from(name);
            let value = self.eval_inner_expr(&declaration.init)?;
            let local_env = self.runtime.local_env_mut();
            local_env.store_value(name, value);
            Ok(value)
        } else {
            unimplemented!()
        }
        
    }
    
    // fn eval_lvalue(&mut self, lvalue: &LValue) -> EvalResult<()> {
        
    // }
}

