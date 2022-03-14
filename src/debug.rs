use std::error::Error;

pub mod symbol;
pub mod dasm;

pub use symbol::DebugSymbol;

mod tests;


/// trait for syntax or compile errors that are directly related to a piece of source code
pub trait SourceError: Error {
    fn debug_symbol(&self) -> Option<&DebugSymbol>;
}