use ahash::{self, AHasher};
// use rustc_hash::FxHasher;

mod variant;
pub use variant::{Variant, VariantKey};

pub mod strings;
pub use strings::string_table;

pub mod ops;
pub mod types;
pub mod errors;

mod tests;


// Default Hasher

pub type DefaultHasher = AHasher;
pub type DefaultBuildHasher = ahash::RandomState;

