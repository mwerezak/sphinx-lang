use std::fs;
use std::path::{PathBuf, Path};
use std::io;
use crate::utils::ReadChars;

use crate::lexer::LexerBuilder;
use crate::parser::{Parser, ParserError};
use crate::parser::stmt::StmtMeta;
use crate::runtime::string_table::StringTable;

type ReadFileChars = ReadChars<io::BufReader<fs::File>>;

#[derive(Debug, Clone)]
pub enum SourceType {
    String(String),
    File(PathBuf),
}

pub enum SourceText<'m> {
    String {
        module: &'m ModuleSource,
        text: String,
    },
    File {
        module: &'m ModuleSource,
        text: ReadFileChars
    },
}

// Represents a "source" of source code, and provides the means to access the source text as a sequence of chars
#[derive(Debug, Clone)]
pub struct ModuleSource {
    name: String,
    source: SourceType,
}

impl ModuleSource {
    pub fn new(name: impl ToString, source: SourceType) -> Self {
        ModuleSource {
            name: name.to_string(), 
            source,
        }
    }
    
    pub fn name(&self) -> &str { self.name.as_str() }
    pub fn source(&self) -> &SourceType { &self.source }
    
    // Load the source text
    pub fn source_text(&self) -> io::Result<SourceText> {
        match &self.source {
            SourceType::String(string) => Ok(SourceText::String {
                module: self,
                text: string.clone()
            }),
            SourceType::File(ref path) => Ok(SourceText::File{
                module: self,
                text: Self::read_source_file(path)?,
            }),
        }
    }
    
    fn read_source_file(path: &Path) -> io::Result<ReadFileChars> {
        let file = fs::File::open(path)?;
        let reader = io::BufReader::new(file);
        Ok(ReadChars::new(reader))
    }
    
}

// High-level Parsing Interface

// Container for state required for parsing
pub struct ParseContext<'f, 's> {
    lexer_factory: &'f LexerBuilder,
    string_table: &'s StringTable
}

impl<'f, 's> ParseContext<'f, 's> {
    pub fn new(lexer_factory: &'f LexerBuilder, string_table: &'s StringTable) -> Self {
        ParseContext {
            lexer_factory, string_table,
        }
    }
    
    // Returns a Vec of parsed Stmts (if no error occurred) or a Vec or errors
    pub fn parse_ast<'m>(&mut self, source: SourceText<'m>) -> Result<Vec<StmtMeta>, Vec<ParserError<'m>>> {
        
        let output = self.collect_parser_output(source);
        
        if output.iter().any(|r| r.is_err()) {
            Err(output.into_iter().filter_map(|r| r.err()).collect())
        } else {
            Ok(output.into_iter().filter_map(|r| r.ok()).collect())
        }
    }

    // Helper to deal with the separate branches for parsing SourceText
    fn collect_parser_output<'m>(&mut self, source: SourceText<'m>) -> Vec<Result<StmtMeta, ParserError<'m>>> {
        match source {
            SourceText::String { module, text } => {
                let mut chars = Vec::new();
                chars.extend(text.chars().map(Ok));
                
                let lexer = self.lexer_factory.build(chars.into_iter());
                let mut string_table = self.string_table.borrow_mut();
                let parser = Parser::new(module, &mut string_table, lexer);
                parser.collect()
            }
            SourceText::File { module, text } => {
                let lexer = self.lexer_factory.build(text);
                let mut string_table = self.string_table.borrow_mut();
                let parser = Parser::new(module, &mut string_table, lexer);
                parser.collect()
            },
        }
    }
}