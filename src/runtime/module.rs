///! Modules are the top level environment in Sphinx.
///! All top-level names live in a module, there are no "universal" global variables.
///!
///! They are also the top-level unit of execution. 
///! All Sphinx programs produce a module as a result of execution (even if it is discarded). 
///! Importing a Sphinx module simply means executing a Sphinx sub-program and binding the
///! resulting module to a name.

use crate::runtime::Variant;

type GlobalID = u16; 

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Access {
    ReadOnly,
    ReadWrite,
}

pub struct Global {
    access: Access,
    value: Variant,
}

pub struct Module {
    globals: Box<[Global]>, // within a module, globals are referred to by index
    // names:  subset of globals that are accessible outside of the module
}