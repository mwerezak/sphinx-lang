#![allow(unused_variables)]
#![allow(unused_mut)]

use crate::parser::expr::{ExprMeta, Expr};
use crate::parser::primary::{Primary, Atom};
use crate::parser::assign::{Declaration, Assignment, LValue};

use crate::runtime::{Environment, Variant};
use crate::runtime::strings::StringValue;
use crate::runtime::ops::*;
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::errors::{ExecResult, ErrorKind};


// These will differ more once tracebacks are implemented
pub fn eval_expr<'a>(local_env: &'a Environment<'_, '_>, expr: &ExprMeta) -> ExecResult<Variant> {
    let mut ctx = EvalContext::from(local_env);
    ctx.eval(expr)
}

pub fn eval_expr_variant<'a>(local_env: &'a Environment<'_, '_>, expr: &Expr) -> ExecResult<Variant> {
    let mut ctx = EvalContext::from(local_env);
    ctx.eval_variant(expr)
}

// tracks the local scope and the innermost ExprMeta
pub struct EvalContext<'a, 'r, 's> {
    // very important to keep 'a and 'r separate
    // otherwise EvalContext would be forced to live as long as 'r!
    local_env: &'a Environment<'r, 's>,
}

impl<'a, 'r, 's> From<&'a Environment<'r, 's>> for EvalContext<'a, 'r, 's> {
    fn from(local_env: &'a Environment<'r, 's>) -> Self {
        EvalContext { local_env }
    }
}

impl<'a, 'r, 's> EvalContext<'a, 'r, 's> {
    pub fn eval(&self, expr: &ExprMeta) -> ExecResult<Variant> {
        self.eval_inner_expr(expr.variant())
    }
    
    pub fn eval_variant(&self, expr: &Expr) -> ExecResult<Variant> {
        self.eval_inner_expr(expr)
    }
    
    fn eval_inner_expr(&self, expr: &Expr) -> ExecResult<Variant> {
        match expr {
            Expr::Atom(atom) => self.eval_atom(atom),
            
            Expr::Primary(primary) => self.eval_primary(primary),
            
            Expr::UnaryOp(op, expr) => self.eval_unary_op((*op).into(), expr),
            Expr::BinaryOp(op, exprs) => {
                let (ref lhs, ref rhs) = **exprs;
                self.eval_binary_op((*op).into(), lhs, rhs)
            },
            
            Expr::Assignment(assignment) => self.eval_assignment(assignment),
            Expr::Declaration(declaration) => self.eval_declaration(declaration),
            
            Expr::Tuple(expr_list) => unimplemented!(),
            Expr::ObjectCtor(ctor) => unimplemented!(),
            
            Expr::Block(suite, label) => unimplemented!(),
        }
    }

    fn eval_primary(&self, primary: &Primary) -> ExecResult<Variant> {
        let mut value = self.eval_atom(primary.atom())?;
        
        for item in primary.path().iter() {
            unimplemented!();
        }
        
        Ok(value)
    }

    fn eval_atom(&self, atom: &Atom) -> ExecResult<Variant> {
        let value = match atom {
            Atom::Nil => Variant::Nil,
            Atom::EmptyTuple => Variant::EmptyTuple,
            Atom::BooleanLiteral(true) => Variant::BoolTrue,
            Atom::BooleanLiteral(false) => Variant::BoolFalse,
            Atom::IntegerLiteral(value) => Variant::Integer(*value),
            Atom::FloatLiteral(value) => Variant::Float(*value),
            Atom::StringLiteral(sym) => Variant::String((*sym).into()),
            
            Atom::Identifier(name) => self.find_value(StringValue::from(*name))?,
            
            Atom::Self_ => unimplemented!(),
            Atom::Super => unimplemented!(),
            
            Atom::Group(expr) => self.eval_inner_expr(expr)?,
        };
        Ok(value)
    }
    
    fn find_value(&self, name: StringValue) -> ExecResult<Variant> {
        self.local_env.find_value(name)
            .ok_or_else(|| {
                let mut string = String::new();
                name.write_str(&mut string, self.local_env.string_table()).unwrap();
                ErrorKind::NameNotDefined(string).into()
            })
    }
    
    fn eval_short_circuit_logic(&self, op: Logical, lhs: &Expr, rhs: &Expr) -> ExecResult<Variant> {
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
    
    fn eval_unary_op(&self, op: UnaryOp, expr: &Expr) -> ExecResult<Variant> {
        let operand = self.eval_inner_expr(expr)?;
        
        match op {
            UnaryOp::Neg => eval_neg(&operand),
            UnaryOp::Pos => eval_pos(&operand),
            UnaryOp::Inv => eval_inv(&operand),
            UnaryOp::Not => eval_not(&operand),
        }
    }
    
    fn eval_binary_op(&self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> ExecResult<Variant> {
        if let BinaryOp::Logical(logic) = op {
            return self.eval_short_circuit_logic(logic, lhs, rhs);
        }
        
        let lhs_value = self.eval_inner_expr(lhs)?;
        let rhs_value = self.eval_inner_expr(rhs)?;
        
        self.eval_binary_op_values(op, &lhs_value, &rhs_value)
    }
    
    fn eval_binary_op_values(&self, op: BinaryOp, lhs: &Variant, rhs: &Variant) -> ExecResult<Variant> {
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
    fn eval_binary_from_type(_op: BinaryOp, _lhs: &Variant, _rhs: &Variant) -> ExecResult<Variant> {
        // TODO defer to lhs's type metamethods, or rhs's type reflected metamethod
        unimplemented!()
    }
    
    fn find_env(&'a self, name: StringValue) -> ExecResult<&'a Environment<'r, 's>> {
        self.local_env.find_name(name)
            .ok_or_else(|| {
                let mut string = String::new();
                name.write_str(&mut string, self.local_env.string_table()).unwrap();
                ErrorKind::NameNotDefined(string).into()
            })
    }
    
    fn eval_assignment(&self, assignment: &Assignment) -> ExecResult<Variant> {
        if let LValue::Identifier(name) = assignment.lhs {
            let name = StringValue::from(name);
            let store_env = self.find_env(name)?;
            
            let lhs_value = store_env.lookup_value(name).unwrap();
            let mut rhs_value = self.eval_inner_expr(&assignment.rhs)?;
            
            if let Some(op) = assignment.op {
                rhs_value = self.eval_binary_op_values(op, &lhs_value, &rhs_value)?;
            }
            
            store_env.insert_value(name, rhs_value);
            Ok(rhs_value)
        } else {
            unimplemented!()
        }
    }
    
    fn eval_declaration(&self, declaration: &Declaration) -> ExecResult<Variant> {
        
        if let LValue::Identifier(name) = declaration.lhs {
            let name = StringValue::from(name);
            let value = self.eval_inner_expr(&declaration.init)?;
            self.local_env.insert_value(name, value);
            Ok(value)
        } else {
            unimplemented!()
        }
        
    }
    
    // fn eval_lvalue(&mut self, lvalue: &LValue) -> ExecResult<()> {
        
    // }
}

