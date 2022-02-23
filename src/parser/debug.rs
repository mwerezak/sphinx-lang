use crate::lexer::Span;


// metadata attached to parser output for error handling and debug output
// will probably be attached to the statement level


#[derive(Clone, Debug)]
pub struct DebugInfo<'a> {
    pub file: &'a str,
    pub start: Option<Span>,
    pub end: Option<Span>,
}

impl<'a> DebugInfo<'a> {
    pub fn new(file: &'a str, start: Option<Span>, end: Option<Span>) -> Self {
        DebugInfo { file, start, end }
    }
}

pub trait DebugSymbol {
    fn dbg_info(&self) -> &DebugInfo;
    fn dbg_info_mut(&self) -> &mut DebugInfo;
    fn set_dbg_info(&mut self, info: DebugInfo);
}