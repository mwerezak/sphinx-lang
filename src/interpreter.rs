pub mod eval;
pub mod exec;

pub use eval::EvalContext;
pub use exec::ExecContext;

use crate::parser::stmt::Label;
use crate::runtime::Variant;


#[derive(Debug, Clone, Copy)]
pub enum ControlFlow {
    None,
    Continue(Option<Label>),
    Break(Option<Label>, Variant),
    Return(Variant),
}

