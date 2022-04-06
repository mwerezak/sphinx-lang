use crate::parser::lvalue::DeclType;
use crate::codegen::consts::ConstID;
use crate::codegen::opcodes::{LocalIndex, UpvalueIndex};
use crate::runtime::function::Signature;


pub type FunctionID = u16;

#[derive(Debug)]
pub struct FunctionProto {
    pub signature: Signature,
    pub upvalues: Box<[UpvalueTarget]>,
    pub fun_id: FunctionID,
}


#[derive(Debug, Clone)]
pub struct UnloadedFunction {
    pub signature: UnloadedSignature,
    pub upvalues: Box<[UpvalueTarget]>,
    pub fun_id: FunctionID,
}


#[derive(Debug, Clone, Copy)]
pub enum UpvalueTarget {
    Local(LocalIndex),
    Upvalue(UpvalueIndex),
}


#[derive(Clone, Debug)]
pub struct UnloadedSignature {
    pub name: Option<ConstID>,
    pub required: Box<[UnloadedParam]>,
    pub default: Box<[UnloadedParam]>,
    pub variadic: Option<UnloadedParam>,
}


#[derive(Clone, Debug)]
pub struct UnloadedParam {
    pub name: ConstID,
    pub decl: DeclType,
}
