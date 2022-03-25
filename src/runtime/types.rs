use crate::codegen::ChunkID;
use crate::runtime::Variant;
use crate::runtime::module::ModuleID;
use crate::runtime::errors::ExecResult;

pub mod operator;
pub mod metatable;
pub mod primitive;
pub mod function;

pub use metatable::Metatable;


/// Call directive
pub enum Call {
    Chunk(ModuleID, ChunkID),    // the module & chunk to call into
    Native(ExecResult<Variant>), // eagerly computed result
}
