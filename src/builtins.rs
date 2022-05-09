use crate::runtime::Gc;
use crate::runtime::module::NamespaceEnv;

mod iter;
mod primitive;
mod misc;

use iter::create_iter_builtins;
use primitive::{create_primitive_ctors, create_metamethod_builtins};
use misc::create_misc_builtins;

// thread_local! {
//     pub static PRELUDE: Gc<NamespaceEnv> = {
//         let prelude = create_prelude();
//         prelude
//     }
// }


/// Create an Env containing the core builtins
/// Fairly expensive, should be used sparingly
pub fn create_prelude() -> Gc<NamespaceEnv> {
    let env = NamespaceEnv::new();
    
    create_metamethod_builtins(env);
    create_primitive_ctors(env);
    create_iter_builtins(env);
    create_misc_builtins(env);
    
    env
}
