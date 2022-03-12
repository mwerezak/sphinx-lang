#![allow(unused_variables)]
#![allow(unused_mut)]

use crate::parser::expr::{ExprMeta, Expr};
use crate::parser::stmt::{StmtMeta, Label};
use crate::parser::primary::{Primary, Atom};
use crate::parser::assign::{Declaration, Assignment, LValue, DeclType};

use crate::runtime::Variant;
use crate::runtime::strings::StringValue;
use crate::runtime::ops::*;
use crate::runtime::types::operator::{UnaryOp, BinaryOp, Arithmetic, Bitwise, Shift, Comparison, Logical};
use crate::runtime::errors::{ExecResult, ErrorKind};

use crate::interpreter::{ControlFlow, ExecContext, Environment, Access};


// evaluation can be interrupted by a control flow statement inside a block expression
#[derive(Debug, Clone)]
pub struct EvalResult(pub Result<Variant, ControlFlow>);

impl From<Variant> for EvalResult {
    fn from(value: Variant) -> Self { Self(Ok(value)) }
}

impl From<ControlFlow> for EvalResult {
    fn from(control: ControlFlow) -> Self { Self(Err(control)) }
}

impl EvalResult {
    pub fn is_value(&self) -> bool { self.0.is_ok() }
    pub fn is_control(&self) -> bool { self.0.is_err() }
    
    pub fn unwrap_value(self) -> Variant { self.0.unwrap() }
    pub fn unwrap_control(self) -> ControlFlow { self.0.unwrap_err() }
    
    pub fn value(self) -> Option<Variant> { self.0.ok() }
    pub fn control(self) -> Option<ControlFlow> {
        match self.0 {
            Err(control) => Some(control),
            Ok(..) => None,
        }
    }
}

// for use with functions that return -> Result<EvalResult, _>
macro_rules! try_value {
    ( $expr:expr ) => {
        match $expr {
            EvalResult(Ok(value)) => value,
            EvalResult(Err(control)) => return Ok(control.into()),
        }
    };
}
pub (crate) use try_value;


// tracks the local scope and the innermost ExprMeta
pub struct EvalContext<'a, 'r, 's> {
    // very important to keep 'a and 'r separate
    // otherwise EvalContext would be forced to live as long as 'r!
    local_env: &'a Environment<'r, 's>,
}

impl<'a, 'r, 's> EvalContext<'a, 'r, 's> {
    pub fn new(local_env: &'a Environment<'r, 's>) -> Self {
        EvalContext { local_env }
    }
    
    pub fn eval(&self, expr: &ExprMeta) -> ExecResult<EvalResult> {
        self.eval_expr(expr.variant())
    }
    
    pub fn eval_expr(&self, expr: &Expr) -> ExecResult<EvalResult> {
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
            
            Expr::Tuple(expr_list) => self.eval_tuple(expr_list),
            Expr::ObjectCtor(ctor) => unimplemented!(),
            
            Expr::Block(label, suite) => self.eval_block(label, suite.iter()),
            
            Expr::FunctionDef(fundef) => unimplemented!(),
        }
    }

    fn eval_block<'b>(&self, block_label: &Option<Label>, suite: impl Iterator<Item=&'b StmtMeta>) -> ExecResult<EvalResult> {
        let new_env = self.local_env.new_local();
        let block_ctx = ExecContext::new(&new_env);
        
        let mut result = None;
        for stmt in suite {
            let control = block_ctx.exec(&stmt)?;
            match control {
                // these aren't used by block expressions and so must relate to an enclosing item
                ControlFlow::Continue(..) | ControlFlow::Return(..) => return Ok(control.into()),
                
                ControlFlow::Break(target_label, value) => {
                    // if a target label was supplied, check to see if it matches this block's label
                    if let Some(target) = target_label {
                        if block_label.map_or(true, |block| target != block) {
                            return Ok(ControlFlow::Break(target_label, value).into())
                        }
                    }
                    
                    // if we get here, the break applies to this block
                    result.replace(value);
                    break;
                }
                
                ControlFlow::None => { }
            }
        }

        Ok(result.unwrap_or(Variant::Nil).into())
    }

    fn eval_primary(&self, primary: &Primary) -> ExecResult<EvalResult> {
        let mut value = try_value!(self.eval_atom(primary.atom())?);
        
        for item in primary.path().iter() {
            unimplemented!();
        }
        
        Ok(value.into())
    }

    fn eval_atom(&self, atom: &Atom) -> ExecResult<EvalResult> {
        let value = match atom {
            Atom::Nil => Variant::Nil,
            Atom::EmptyTuple => Variant::EmptyTuple,
            Atom::BooleanLiteral(true) => Variant::BoolTrue,
            Atom::BooleanLiteral(false) => Variant::BoolFalse,
            Atom::IntegerLiteral(value) => Variant::Integer(*value),
            Atom::FloatLiteral(value) => Variant::Float(*value),
            Atom::StringLiteral(sym) => Variant::String((*sym).into()),
            
            Atom::Identifier(name) => self.local_env.lookup(&StringValue::from(*name))?,
            
            Atom::Self_ => unimplemented!(),
            Atom::Super => unimplemented!(),
            
            Atom::Group(expr) => return self.eval_expr(expr),
        };
        Ok(value.into())
    }
    
    fn eval_tuple(&self, expr_list: &[ExprMeta]) -> ExecResult<EvalResult> {
        let mut items = Vec::with_capacity(expr_list.len());
        for expr in expr_list.iter() {
            items.push(try_value!(self.eval_expr(expr.variant())?));
        }
        Ok(Variant::make_tuple(items.into_boxed_slice()).into())
    }
    
    fn eval_short_circuit_logic(&self, op: Logical, lhs: &Expr, rhs: &Expr) -> ExecResult<EvalResult> {
        let lhs_value = try_value!(self.eval_expr(lhs)?);
        
        let cond = match op {
            Logical::And => !lhs_value.truth_value(),
            Logical::Or => lhs_value.truth_value(),
        };
        
        if cond {
            Ok(lhs_value.into())
        } else {
            Ok(try_value!(self.eval_expr(rhs)?).into())
        }
    }
    
    fn eval_unary_op(&self, op: UnaryOp, expr: &Expr) -> ExecResult<EvalResult> {
        let operand = try_value!(self.eval_expr(expr)?);
        
        let result = match op {
            UnaryOp::Neg => eval_neg(&operand)?,
            UnaryOp::Pos => eval_pos(&operand)?,
            UnaryOp::Inv => eval_inv(&operand)?,
            UnaryOp::Not => Some(eval_not(&operand)?),
        };
        
        if let Some(value) = result {
            return Ok(value.into());
        }
        
        unimplemented!();
    }
    
    fn eval_binary_op(&self, op: BinaryOp, lhs: &Expr, rhs: &Expr) -> ExecResult<EvalResult> {
        if let BinaryOp::Logical(logic) = op {
            return self.eval_short_circuit_logic(logic, lhs, rhs);
        }
        
        let lhs_value = try_value!(self.eval_expr(lhs)?);
        let rhs_value = try_value!(self.eval_expr(rhs)?);
        
        self.eval_binary_op_values(op, &lhs_value, &rhs_value).map(|value| value.into())
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
                
                Comparison::EQ     => eval_eq(&lhs, &rhs),
                Comparison::NE     => eval_ne(&lhs, &rhs),
            
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
    
    fn eval_assignment(&self, assignment: &Assignment) -> ExecResult<EvalResult> {
        if let LValue::Identifier(name) = assignment.lhs {
            let name = StringValue::from(name);
            
            // make sure we evaluate rhs *before* borrowing lhs
            let mut rhs_value = try_value!(self.eval_expr(&assignment.rhs)?);
            
            let mut lhs_value;
            
            if assignment.global {
                lhs_value = self.local_env.lookup_mut(&name)?;
                
            } else {
                let lookup_result = self.local_env.lookup_local_mut(&name);
                
                // produce a different error if the name isn't defined locally but *is* defined in some enclosing scope
                match lookup_result {
                    Err(error) if matches!(error.kind(), ErrorKind::NameNotDefinedLocal(..)) => {
                        self.local_env.lookup(&name)?; // see if the name is defined at all
                        return Err(ErrorKind::CantAssignNonLocal.into());
                    }
                    _ => lhs_value = lookup_result?,
                }
                
            }
            
            if let Some(op) = assignment.op {
                rhs_value = self.eval_binary_op_values(op, &lhs_value, &rhs_value)?;
            }
            
            *lhs_value = rhs_value.clone();
            
            Ok(rhs_value.into())
        } else {
            unimplemented!()
        }
    }
    
    fn eval_declaration(&self, declaration: &Declaration) -> ExecResult<EvalResult> {
        
        if let LValue::Identifier(name) = declaration.lhs {
            let name = StringValue::from(name);
            
            let access = match declaration.decl {
                DeclType::Immutable => Access::ReadOnly,
                DeclType::Mutable => Access::ReadWrite,
            };
            
            let init_value = try_value!(self.eval_expr(&declaration.init)?);
            
            self.local_env.create(&name, access, init_value.clone())?;
            
            Ok(init_value.into())
        } else {
            unimplemented!()
        }
    }
    
    // fn eval_declaration_inner(&self, name: &StringSymbol, access: Access, init: &Expr) -> ExecResult<EvalResult> {
        
    // }
    
    // fn eval_lvalue(&mut self, lvalue: &LValue) -> ExecResult<()> {
        
    // }
}

