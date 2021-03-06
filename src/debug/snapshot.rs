use core::fmt::{Display, Formatter, Result};
use crate::codegen::opcodes::OpCode;
use crate::runtime::Variant;
use crate::runtime::module::Chunk;


pub struct VMSnapshot {
    pub calls: Vec<VMFrameSnapshot>,
    pub frame: VMFrameSnapshot,
    pub stack: Vec<Variant>,
    pub locals: Vec<Variant>,
}

impl Display for VMSnapshot {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        
        writeln!(fmt, "== Call Stack ==")?;
        
        for (idx, state) in self.calls.iter().enumerate() {
            write!(fmt, "{: >4}: Module: {}, Chunk: ", idx, state.module)?;
            format_chunk_id(fmt,state.chunk_id)?;
            writeln!(fmt, ", Frame: {}, Locals: {}", state.stack_idx, state.local_idx)?;
        }
        
        write!(fmt, "{: >4}: Module: {}, Chunk: ", self.calls.len(), self.frame.module)?;
        format_chunk_id(fmt, self.frame.chunk_id)?;
        writeln!(fmt, ", Frame: {}, Locals: {}", self.frame.stack_idx, self.frame.local_idx)?;
        
        writeln!(fmt, "\n== Locals ==")?;
        for (idx, chunk) in self.locals.chunks(10).enumerate() {
            write!(fmt, "{: >4}: ", idx * 10)?;
            let items = chunk.iter().map(|value| format!("{:?}", value))
                .collect::<Vec<String>>()
                .join(", ");
            writeln!(fmt, "{}", items)?;
        }
        
        writeln!(fmt, "\n== Temporaries ==")?;
        for (idx, chunk) in self.stack.chunks(10).enumerate() {
            write!(fmt, "{: >4}: ", idx * 10)?;
            let items = chunk.iter().map(|value| format!("{:?}", value))
                .collect::<Vec<String>>()
                .join(", ");
            writeln!(fmt, "{}", items)?;
        }
        
        writeln!(fmt, "\n== Active Frame ==")?;
        writeln!(fmt, "{}", self.frame)?;
        
        Ok(())
    }
}


pub struct VMFrameSnapshot {
    pub module: String,
    pub chunk_id: Chunk,
    pub stack_idx: usize,
    pub local_idx: usize,
    pub pc: usize,
    pub next_instr: Option<Vec<u8>>,
}

impl Display for VMFrameSnapshot {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        
        write!(fmt, "Module: {}, Chunk: ", self.module)?;
        format_chunk_id(fmt, self.chunk_id)?;
        writeln!(fmt)?;
        
        writeln!(fmt, "Frame: {}, Locals: {}", self.stack_idx, self.local_idx)?;
        
        write!(fmt, "PC: {:#X}", self.pc)?;
        if let Some(instr) = self.next_instr.as_ref() {
            let opcode = OpCode::try_from(instr[0])
                .map_or("INVALID".to_string(), |opcode| format!("{}", opcode));
            
            let bytes = instr.iter()
                .map(|b| format!("{:0>2X}", b))
                .collect::<Vec<String>>()
                .join(" ");
            
            write!(fmt, ", Next: {} [{}]", opcode, bytes)?;
        }
        writeln!(fmt)?;
        
        Ok(())
    }
}

fn format_chunk_id(fmt: &mut Formatter<'_>, chunk_id: Chunk) -> Result {
    match chunk_id {
        Chunk::Main => fmt.write_str("<main>"),
        Chunk::Function(fun_id) => write!(fmt, "{}", fun_id),
    }
}

