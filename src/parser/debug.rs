use crate::lexer::Span;


// Metadata attached to all parser output for error handling and debug output
#[derive(Clone, Debug)]
pub struct DebugMeta<'a> {
    pub file: &'a str,
    pub start: Option<Span>,
    pub end: Option<Span>,
}



pub trait DebugInfo {
    fn dbg_info(&self) -> &DebugMeta;
    fn dbg_info_mut(&self) -> &mut DebugMeta;
    fn set_dbg_info(&mut self, info: DebugMeta);
}