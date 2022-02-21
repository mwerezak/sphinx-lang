use crate::lexer::Span;


// metadata attached to parser output for error handling and debug output

// attached at the expression and statment level: 
// each Expr and Stmt outputted from the parser should have a DebugMeta


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