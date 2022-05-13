use crate::codegen::OpCode;
use crate::debug::snapshot::VMFrameSnapshot;
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::module::{Module, Chunk, FunctionID};


#[derive(Debug)]
pub struct VMCallFrame<'c> {
    pub(super) module: Gc<Module>,
    pub(super) chunk: &'c [u8],
    pub(super) chunk_id: Chunk,
    pub(super) stack_idx: usize,   // start index for this frame in the value stack
    pub(super) local_idx: usize,   // start index for this frame in the locals stack
    pub(super) pc: usize,
}

unsafe impl GcTrace for VMCallFrame<'_> {
    fn trace(&self) {
        self.module.mark_trace();
    }
}

impl<'c> VMCallFrame<'c> {
    pub fn call_frame(module: Gc<Module>, fun_id: FunctionID, stack_idx: usize, local_idx: usize) -> Self {
        
        // This hack allows us to get around the self-referentiality of storing both "module" and "chunk"
        // in the same struct. The alternative would be to store "chunk" as an `Option<Box<[u8]>>` and call 
        // `module.data().get_chunk()` before every single instruction, but I want to avoid the overhead of that.
        // SAFETY: This is safe because "module" is rooted as long as this VMCallFrame is in the call stack, 
        // and VMCallFrames are never stored outside of a VirtualMachine's call stack.
        let chunk: *const [u8] = module.data().get_chunk(fun_id);
        let chunk = unsafe { chunk.as_ref::<'c>().unwrap() };
        
        Self {
            module,
            chunk,
            chunk_id: Chunk::Function(fun_id),
            stack_idx,
            local_idx,
            pc: 0,
        }
    }
    
    pub fn main_chunk(module: Gc<Module>, chunk: &'c [u8]) -> Self {
        Self {
            module,
            chunk,
            chunk_id: Chunk::Main,
            stack_idx: 0,
            local_idx: 0,
            pc: 0,
        }
    }
    
    #[inline]
    pub fn stack_frame(&self) -> usize { self.stack_idx }

    #[inline]
    pub fn local_frame(&self) -> usize { self.local_idx }

    #[inline]
    pub fn module(&self) -> Gc<Module> { self.module }

}


// debugging

impl From<&VMCallFrame<'_>> for VMFrameSnapshot {
    fn from(state: &VMCallFrame) -> Self {
        let next_instr = state.chunk.get(state.pc)
            .map(|byte| OpCode::try_from(*byte).map_or_else(
                |byte| vec![ byte ],
                |opcode| state.chunk[state.pc..(state.pc + opcode.instr_len())].to_vec()
            ));
        
        Self {
            module: state.module.to_string(),
            chunk_id: state.chunk_id,
            stack_idx: state.stack_idx,
            local_idx: state.local_idx,
            pc: state.pc,
            next_instr,
        }
    }
}