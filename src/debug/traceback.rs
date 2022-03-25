use crate::runtime::module::{ModuleID, ChunkID};

/// Traceback information
#[derive(Debug, Clone)]
pub enum CallSite {
    Chunk {
        offset: usize,
        module_id: ModuleID,
        chunk_id: Option<ChunkID>,
    },
    Native,
}