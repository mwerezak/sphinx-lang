use std::fmt;
use crate::source::ModuleSource;
use crate::codegen::chunk::ChunkInfo;
use crate::runtime::gc::{GC, GCTrace};
use crate::runtime::module::{Module, Chunk};


/// Traceback information
#[derive(Debug, Clone)]
pub enum TraceSite {
    Chunk {
        offset: usize,
        module: GC<Module>,
        chunk_id: Chunk,
    },
    Native,  // TODO reference native function?
}

unsafe impl GCTrace for TraceSite {
    fn trace(&self) {
        if let Self::Chunk { module, .. } = self {
            module.mark_trace();
        }
    }
}


pub struct FrameSummary<'a> {
    trace: &'a TraceSite,
}

impl fmt::Display for FrameSummary<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.trace {
            TraceSite::Chunk { offset, module, chunk_id } => {
                let module_desc = module_desc(module);
                let loc_desc = format!("<@{:#X}>", offset);
                let chunk_desc = chunk_desc(module, chunk_id);
                write!(fmt, "{}, {} in {}", module_desc, loc_desc, chunk_desc)
            },
            
            TraceSite::Native => {
                write!(fmt, "<native code>")
            },
        }
    }
}

fn module_desc(module: &Module) -> String {
    if let Some(ModuleSource::File(path)) = module.source() {
        format!("File \"{}\"", path.display())
    } else {
        "<anonymous module>".to_string()
    }
}

fn chunk_desc(module: &Module, chunk_id: &Chunk) -> String {
    match chunk_id {
        Chunk::Main => "<module>".to_string(),
        
        Chunk::Function(fun_id) => {
            let function = module.data().get_function(*fun_id);
            format!("{}", function.signature())
        },
    }
}


pub struct Traceback<'a> {
    frames: Vec<FrameSummary<'a>>,
}


impl<'a> Traceback<'a> {
    pub fn build(trace: impl Iterator<Item=&'a TraceSite>) -> Self {
        Self {
            frames: trace.map(|trace| FrameSummary { trace }).collect(),
        }
    }
}

impl fmt::Display for Traceback<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str("Stack trace (most recent call last):\n")?;
        
        for (idx, frame) in self.frames.iter().enumerate().rev() {
            writeln!(fmt, "#{} {}", idx + 1, frame)?;
        }
        
        Ok(())
    }
}
