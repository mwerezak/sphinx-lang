use std::fs;
use std::path::PathBuf;
use std::io;
use crate::utils::ReadChars;

type ReadFileChars = ReadChars<io::BufReader<fs::File>>;

#[derive(Debug, Clone)]
pub enum SourceType {
    String(String),
    File(PathBuf),
}

pub enum SourceText {
    String(String),
    File(ReadFileChars),
}

// Represents a "source" of source code, and provides the means to access the source text as a sequence of chars
#[derive(Debug, Clone)]
pub struct ModuleSource {
    name: String,
    source: SourceType,
}

impl ModuleSource {
    pub fn new<S: ToString>(name: S, source: SourceType) -> Self {
        ModuleSource {
            name: name.to_string(), 
            source,
        }
    }
    
    // Load the source text
    pub fn source_text(&self) -> io::Result<SourceText> {
        match &self.source {
            SourceType::String(string) => Ok(SourceText::String(string.clone())),
            SourceType::File(ref path) => Ok(SourceText::File(Self::read_source_file(path)?)),
        }
    }
    
    fn read_source_file(path: &PathBuf) -> io::Result<ReadFileChars> {
        let file = fs::File::open(path)?;
        let reader = io::BufReader::new(file);
        Ok(ReadChars::new(reader))
    }
    
}