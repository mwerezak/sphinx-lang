
use crate::language;
use crate::runtime::data::InternStr;



// Fundamental data value type
#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Nil,
    EmptyTuple, // the empty tuple value
    Boolean(bool),
    Integer(language::IntType),
    Float(language::FloatType),
    InternStr(InternStr),
    //GCObject(GCHandle),
}
