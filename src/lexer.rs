mod token;
mod lexer;
mod errors;

pub mod rules;
pub use rules::MatchResult;

pub use token::*;
pub use lexer::*;
pub use errors::*;

mod tests;