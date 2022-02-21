use crate::lexer::TokenMeta;


#[derive(Clone, Debug)]
pub struct TokenSpan {
    start: Option<Box<TokenMeta>>,
    end: Option<Box<TokenMeta>>,
}

impl TokenSpan {
    pub fn new() -> Self {
        TokenSpan { start: None, end: None, }
    }
    
    pub fn start(&self) -> Option<&TokenMeta> { self.start.as_ref().map(|inner| inner.as_ref()) }
    pub fn end(&self) -> Option<&TokenMeta> { self.end.as_ref().map(|inner| inner.as_ref()) }
    
    pub fn set_start(&mut self, token: TokenMeta) { self.start.replace(Box::new(token)); }
    pub fn set_end(&mut self, token: TokenMeta) { self.end.replace(Box::new(token)); }
}


// Metadata attached to all parser output for error handling and debug output
pub struct DebugMeta<'a> {
    file: &'a str,
    span: TokenSpan,
}


pub trait DebugInfo {
    fn dbg_info(&self) -> &DebugMeta;
    fn dbg_info_mut(&self) -> &mut DebugMeta;
    fn set_dbg_info(&mut self, info: DebugMeta);
}