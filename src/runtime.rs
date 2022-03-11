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



// #[derive(Debug)]
// pub struct Runtime<'r, 's> {
//     string_table: &'s StringTable,
    
//     // TODO when modules are implemented this will be replaced
//     root_env: Environment<'r, 's>,
// }

// impl<'s> Runtime<'_, 's> {
//     pub fn new(string_table: &'s StringTable) -> Self {
//         let root_env = Environment { 
//             string_table, 
//             namespace: new_namespace(),
//             parent: None,
//         };
        
//         Runtime {
//             string_table,
//             root_env,
//         }
//     }
    
//     pub fn string_table(&self) -> &StringTable { self.string_table }
    
// }
