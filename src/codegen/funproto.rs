use crate::parser::lvalue::DeclType;
use crate::codegen::consts::ConstID;
use crate::codegen::opcodes::{LocalIndex, UpvalueIndex};
use crate::runtime::function::Signature;


pub type FunctionID = u16;

#[derive(Debug)]
pub struct FunctionProto {
    signature: Signature,
    upvalues: Box<[UpvalueTarget]>,
    fun_id: FunctionID,
}

impl FunctionProto {
    pub(super) fn new(fun_id: FunctionID, signature: Signature, upvalues: Box<[UpvalueTarget]>) -> Self {
        Self {
            fun_id,
            signature,
            upvalues,
        }
    }
    
    pub fn fun_id(&self) -> FunctionID { self.fun_id }
    pub fn signature(&self) -> &Signature { &self.signature }
    pub fn upvalues(&self) -> &[UpvalueTarget] { &self.upvalues }
}


#[derive(Debug, Clone, Copy)]
pub enum UpvalueTarget {
    Local(LocalIndex),
    Upvalue(UpvalueIndex),
}


#[derive(Debug, Clone)]
pub struct UnloadedFunction {
    pub signature: UnloadedSignature,
    pub upvalues: Box<[UpvalueTarget]>,
    pub fun_id: FunctionID,
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
