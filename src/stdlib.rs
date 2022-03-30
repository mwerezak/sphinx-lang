use crate::runtime::module::{Namespace, GlobalEnv};

mod prelude;

use prelude::create_prelude;

thread_local! {
    static PRELUDE: Namespace = create_prelude();
}

pub fn prelude_env() -> GlobalEnv {
    PRELUDE.with(|prelude| GlobalEnv::from(prelude.clone()))
}