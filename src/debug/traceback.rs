use crate::runtime::gc::GC;
use crate::runtime::module::{Module, ChunkID};

/// Traceback information
#[derive(Debug, Clone)]
pub enum CallSite {
    Chunk {
        offset: usize,
        module: GC<Module>,
        chunk_id: Option<ChunkID>,
    },
    Native,
}