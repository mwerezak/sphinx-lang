use crate::language;
use crate::lexer::LexerBuilder;
use crate::runtime::data::StringInterner;

pub struct Runtime {
    pub interner: StringInterner,
    pub lexer_factory: LexerBuilder,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            interner: StringInterner::new(),
            lexer_factory: language::create_default_lexer_rules(),
        }
    }
    
    pub fn interner_mut(&mut self) -> &mut StringInterner { &mut self.interner }
}


pub struct Scope<'r> {
    pub runtime: &'r Runtime,
}

