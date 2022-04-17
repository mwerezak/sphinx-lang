use ahash::{self, AHasher};
// use rustc_hash::FxHasher;

mod variant;
pub mod strings;
pub mod types;
pub mod gc;
pub mod vm;
pub mod function;
pub mod module;
pub mod errors;

mod tests;

pub use gc::Gc;
pub use vm::VirtualMachine;
pub use strings::STRING_TABLE;
pub use variant::{Variant, VariantKey};
pub use module::Module;
pub use errors::{RuntimeError, ExecResult};

// Default Hasher

pub type DefaultHasher = AHasher;
pub type DefaultBuildHasher = ahash::RandomState;
pub type HashMap<K, V> = std::collections::HashMap<K,V, DefaultBuildHasher>;