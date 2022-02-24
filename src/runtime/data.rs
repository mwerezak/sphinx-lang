use crate::language;
use crate::runtime::InternStr;


// Fundamental data value type
#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Nil,
    Boolean(bool),
    Integer(language::IntType),
    Float(language::FloatType),
    InternString(InternStr),
    //GCObject(Box<..>),
}
