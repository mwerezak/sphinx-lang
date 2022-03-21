use ahash::{self, AHasher};
// use rustc_hash::FxHasher;

mod variant;
pub mod strings;
pub mod ops;
pub mod types;
pub mod gc;
pub mod vm;
pub mod module;
pub mod errors;

mod tests;

pub use strings::STRING_TABLE;
pub use variant::{Variant, VariantKey};
pub use vm::VirtualMachine;
pub use module::{ModuleCache, Module, ModuleID};
pub use errors::{RuntimeError, ExecResult};

// Default Hasher

pub type DefaultHasher = AHasher;
pub type DefaultBuildHasher = ahash::RandomState;
