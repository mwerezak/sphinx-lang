use crate::parser::stmt::Label;
use crate::runtime::Variant;

pub mod eval;
pub mod exec;

pub use eval::EvalContext;
pub use exec::ExecContext;


#[derive(Debug, Clone)]
pub enum ControlFlow {
    None,
    Continue(Option<Label>),
    Break(Option<Label>, Variant),
    Return(Variant),
}

