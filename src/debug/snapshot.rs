use std::fmt::{Display, Formatter, Result};
use crate::source::ModuleSource;
use crate::codegen::opcodes::OpCode;
use crate::runtime::Variant;
use crate::runtime::vm::LocalIndex;
use crate::runtime::module::ChunkID;


pub struct VMSnapshot {
    pub calls: Vec<VMStateSnapshot>,
    pub values: Vec<Variant>,
    pub state: VMStateSnapshot,
}

impl Display for VMSnapshot {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        
        writeln!(fmt, "== Call Stack ==")?;
        
        for (idx, state) in self.calls.iter().enumerate() {
            write!(fmt, "{: >4}: Module: {}, Chunk: ", idx, state.module)?;
            format_chunk_id(fmt,state.chunk_id)?;
            writeln!(fmt, ", Frame: {}, Locals: {}", state.frame, state.locals)?;
        }
        
        write!(fmt, "{: >4}: Module: {}, Chunk: ", self.calls.len(), self.state.module)?;
        format_chunk_id(fmt, self.state.chunk_id)?;
        writeln!(fmt, ", Frame: {}, Locals: {}", self.state.frame, self.state.locals)?;
        
        writeln!(fmt, "\n== Value Stack ==")?;
        for (idx, chunk) in self.values.chunks(10).enumerate() {
            write!(fmt, "{: >4}: ", idx * 10)?;
            let items = chunk.iter().map(|value| format!("{:?}", value))
                .collect::<Vec<String>>()
                .join(", ");
            writeln!(fmt, "{}", items)?;
        }
        
        writeln!(fmt, "\n== State ==")?;
        writeln!(fmt, "{}", self.state)?;
        
        Ok(())
    }
}


pub struct VMStateSnapshot {
    pub module: String,
    pub chunk_id: Option<ChunkID>,
    pub frame: usize,
    pub locals: LocalIndex,
    pub pc: usize,
    pub next_instr: Option<Vec<u8>>,
}

impl Display for VMStateSnapshot {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result {
        
        write!(fmt, "Module: {}, Chunk: ", self.module)?;
        format_chunk_id(fmt, self.chunk_id)?;
        writeln!(fmt)?;
        
        writeln!(fmt, "Frame: {}, Locals: {}", self.frame, self.locals)?;
        
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

fn format_chunk_id(fmt: &mut Formatter<'_>, chunk_id: Option<ChunkID>) -> Result {
    if let Some(chunk_id) = chunk_id {
        write!(fmt, "{}", chunk_id)
    } else {
        fmt.write_str("<main>")
    }
}

