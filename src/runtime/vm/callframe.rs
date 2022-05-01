use crate::codegen::{OpCode, LocalIndex};
use crate::debug::snapshot::VMFrameSnapshot;
use crate::runtime::gc::{Gc, GcTrace};
use crate::runtime::module::{Module, Chunk, FunctionID};

/*
    Note: value stack segmentation
    
    Each `VMCallFrame` has a starting position in the stack that is tracked by `VMCallFrame::frame_idx`
    The first section of the frame is the local variables, and the length of this section is tracked by `VMCallFrame::locals`
    Everything after (until the start of the next frame) are temporaries.
    
    With the execption of frame #0,
    The first local (index 0) is always the callable that is being invoked in that frame
    The second local (index 1) is always the number of arguments (nargs)
    The following N locals (N=nargs) are the argument values for the invoked callable.
    
    When inserting new locals, any temporaries at the end of the value stack will get shifted.
    It is expected that the number of temporaries at the end of the last frame will always be small
    (in fact, unless a declaration occurs inside an expression there will only be a single temporary)
    so the performance impact will be negligible.
    
    Example:
    
    L = locals
    T = temporaries
    
     |--frame #0---|-------frame #1--------|-------frame #2------|
     |----L----|-T-|-------L-------|---T---|--L--|-------T-------|
    [ 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 ]
*/


#[derive(Debug)]
pub struct VMCallFrame<'c> {
    pub(super) module: Gc<Module>,
    pub(super) chunk: &'c [u8],
    pub(super) chunk_id: Chunk,
    pub(super) frame_idx: usize,   // start index for this frame in the value stack
    pub(super) locals: LocalIndex, // local variable count
    pub(super) pc: usize,
}

unsafe impl GcTrace for VMCallFrame<'_> {
    fn trace(&self) {
        self.module.mark_trace();
    }
}

impl<'c> VMCallFrame<'c> {
    pub fn call_frame(module: Gc<Module>, fun_id: FunctionID, frame_idx: usize, locals: LocalIndex) -> Self {
        
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
            frame_idx,
            locals,
            pc: 0,
        }
    }
    
    pub fn main_chunk(module: Gc<Module>, chunk: &'c [u8]) -> Self {
        Self {
            module,
            chunk,
            chunk_id: Chunk::Main,
            frame_idx: 0,
            locals: 0,
            pc: 0,
        }
    }
    
    #[inline]
    pub fn start_index(&self) -> usize { self.frame_idx }
    
    #[inline]
    pub fn locals(&self) -> LocalIndex { self.locals }

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
            frame_idx: state.frame_idx,
            locals: state.locals,
            pc: state.pc,
            next_instr,
        }
    }
}