use crate::runtime::Variant;
use crate::runtime::ops::eval_not;
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


type Method0<R> = fn(&Variant) -> ExecResult<R>;
type Method1<R> = fn(&Variant, &Variant) -> ExecResult<R>;
// type Method2<R> = fn(&Variant, &Variant, &Variant) -> ExecResult<R>;

// pub type MethodSelf = Method0<Variant>;
// pub type MethodArg1 = Method1<Variant>;
// pub type MethodArg2 = Method2<Variant>;

pub type MethodUnary = Method0<Variant>;
pub type MethodBinary = Method1<Option<Variant>>; // a None result will cause the reflected version to be called
pub type MethodBinaryReflected = Method1<Variant>;
pub type MethodCompare = Method1<Option<bool>>;

#[derive(Default)]
pub struct Metatable {
    // Operator Overloads
    op_pos: Option<MethodUnary>,
    op_neg: Option<MethodUnary>,
    op_inv: Option<MethodUnary>,
    
    op_add: Option<MethodBinary>,
    op_sub: Option<MethodBinary>,
    op_mul: Option<MethodBinary>,
    op_div: Option<MethodBinary>,
    op_mod: Option<MethodBinary>,
    op_and: Option<MethodBinary>,
    op_xor: Option<MethodBinary>,
    op_or:  Option<MethodBinary>,
    op_shl: Option<MethodBinary>,
    op_shr: Option<MethodBinary>,
    
    op_radd: Option<MethodBinaryReflected>,
    op_rsub: Option<MethodBinaryReflected>,
    op_rmul: Option<MethodBinaryReflected>,
    op_rdiv: Option<MethodBinaryReflected>,
    op_rmod: Option<MethodBinaryReflected>,
    op_rand: Option<MethodBinaryReflected>,
    op_rxor: Option<MethodBinaryReflected>,
    op_ror:  Option<MethodBinaryReflected>,
    op_rshl: Option<MethodBinaryReflected>,
    op_rshr: Option<MethodBinaryReflected>,
    
    cmp_lt: Option<MethodCompare>,
    cmp_le: Option<MethodCompare>,
    cmp_eq: Option<MethodCompare>,
}


// Metamethod Tags


// Operator Overloads
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryTag {
    Pos, Neg, Inv,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryTag {
    Add, Sub,
    Mul, Div, Mod,
    And, Xor, Or, 
    Shl, Shr,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompareTag{
    LT, LE, EQ,
}

impl Metatable {
    #[inline]
    pub fn op_unary(&self, tag: UnaryTag) -> Option<&MethodUnary> {
        match tag {
            UnaryTag::Pos => self.op_pos.as_ref(),
            UnaryTag::Neg => self.op_neg.as_ref(),
            UnaryTag::Inv => self.op_inv.as_ref(),
        }
    }
    
    pub fn set_unary(&mut self, tag: UnaryTag, method: MethodUnary) -> Option<MethodUnary> {
        match tag {
            UnaryTag::Pos => self.op_pos.replace(method),
            UnaryTag::Neg => self.op_neg.replace(method),
            UnaryTag::Inv => self.op_inv.replace(method),
        }
    }
    
    pub fn take_unary(&mut self, tag: UnaryTag) -> Option<MethodUnary> {
        match tag {
            UnaryTag::Pos => self.op_pos.take(),
            UnaryTag::Neg => self.op_neg.take(),
            UnaryTag::Inv => self.op_inv.take(),
        }
    }
    
    #[inline]
    pub fn op_binary(&self, tag: BinaryTag) -> Option<&MethodBinary> {
        match tag {
            BinaryTag::Add => self.op_add.as_ref(),
            BinaryTag::Sub => self.op_sub.as_ref(),
            BinaryTag::Mul => self.op_mul.as_ref(),
            BinaryTag::Div => self.op_div.as_ref(),
            BinaryTag::Mod => self.op_mod.as_ref(),
            BinaryTag::And => self.op_and.as_ref(),
            BinaryTag::Xor => self.op_xor.as_ref(),
            BinaryTag::Or  => self.op_or.as_ref(),
            BinaryTag::Shl => self.op_shl.as_ref(),
            BinaryTag::Shr => self.op_shr.as_ref(),
        }
    }
    
    pub fn set_binary(&mut self, tag: BinaryTag, method: MethodBinary) -> Option<MethodBinary> {
        match tag {
            BinaryTag::Add => self.op_add.replace(method),
            BinaryTag::Sub => self.op_sub.replace(method),
            BinaryTag::Mul => self.op_mul.replace(method),
            BinaryTag::Div => self.op_div.replace(method),
            BinaryTag::Mod => self.op_mod.replace(method),
            BinaryTag::And => self.op_and.replace(method),
            BinaryTag::Xor => self.op_xor.replace(method),
            BinaryTag::Or  => self.op_or.replace(method),
            BinaryTag::Shl => self.op_shl.replace(method),
            BinaryTag::Shr => self.op_shr.replace(method),
        }
    }
    
    pub fn take_binary(&mut self, tag: BinaryTag) -> Option<MethodBinary> {
        match tag {
            BinaryTag::Add => self.op_add.take(),
            BinaryTag::Sub => self.op_sub.take(),
            BinaryTag::Mul => self.op_mul.take(),
            BinaryTag::Div => self.op_div.take(),
            BinaryTag::Mod => self.op_mod.take(),
            BinaryTag::And => self.op_and.take(),
            BinaryTag::Xor => self.op_xor.take(),
            BinaryTag::Or  => self.op_or.take(),
            BinaryTag::Shl => self.op_shl.take(),
            BinaryTag::Shr => self.op_shr.take(),
        }
    }
    
    #[inline]
    pub fn op_binary_reflected(&self, tag: BinaryTag) -> Option<&MethodBinaryReflected> {
        match tag {
            BinaryTag::Add => self.op_radd.as_ref(),
            BinaryTag::Sub => self.op_rsub.as_ref(),
            BinaryTag::Mul => self.op_rmul.as_ref(),
            BinaryTag::Div => self.op_rdiv.as_ref(),
            BinaryTag::Mod => self.op_rmod.as_ref(),
            BinaryTag::And => self.op_rand.as_ref(),
            BinaryTag::Xor => self.op_rxor.as_ref(),
            BinaryTag::Or  => self.op_ror.as_ref(),
            BinaryTag::Shl => self.op_rshl.as_ref(),
            BinaryTag::Shr => self.op_rshr.as_ref(),
        }
    }
    
    pub fn set_binary_reflected(&mut self, tag: BinaryTag, method: MethodBinaryReflected) -> Option<MethodBinaryReflected> {
        match tag {
            BinaryTag::Add => self.op_radd.replace(method),
            BinaryTag::Sub => self.op_rsub.replace(method),
            BinaryTag::Mul => self.op_rmul.replace(method),
            BinaryTag::Div => self.op_rdiv.replace(method),
            BinaryTag::Mod => self.op_rmod.replace(method),
            BinaryTag::And => self.op_rand.replace(method),
            BinaryTag::Xor => self.op_rxor.replace(method),
            BinaryTag::Or  => self.op_ror.replace(method),
            BinaryTag::Shl => self.op_rshl.replace(method),
            BinaryTag::Shr => self.op_rshr.replace(method),
        }
    }
    
    pub fn take_binary_reflected(&mut self, tag: BinaryTag) -> Option<MethodBinaryReflected> {
        match tag {
            BinaryTag::Add => self.op_radd.take(),
            BinaryTag::Sub => self.op_rsub.take(),
            BinaryTag::Mul => self.op_rmul.take(),
            BinaryTag::Div => self.op_rdiv.take(),
            BinaryTag::Mod => self.op_rmod.take(),
            BinaryTag::And => self.op_rand.take(),
            BinaryTag::Xor => self.op_rxor.take(),
            BinaryTag::Or  => self.op_ror.take(),
            BinaryTag::Shl => self.op_rshl.take(),
            BinaryTag::Shr => self.op_rshr.take(),
        }
    }
    
    #[inline]
    pub fn op_compare(&self, tag: CompareTag) -> Option<&MethodCompare> {
        match tag {
            CompareTag::LT => self.cmp_lt.as_ref(),
            CompareTag::LE => self.cmp_le.as_ref(),
            CompareTag::EQ => self.cmp_eq.as_ref(),
        }
    }
    
    pub fn set_compare(&mut self, tag: CompareTag, method: MethodCompare) -> Option<MethodCompare> {
        match tag {
            CompareTag::LT => self.cmp_lt.replace(method),
            CompareTag::LE => self.cmp_le.replace(method),
            CompareTag::EQ => self.cmp_eq.replace(method),
        }
    }
    
    pub fn take_compare(&mut self, tag: CompareTag) -> Option<MethodCompare> {
        match tag {
            CompareTag::LT => self.cmp_lt.take(),
            CompareTag::LE => self.cmp_le.take(),
            CompareTag::EQ => self.cmp_eq.take(),
        }
    }
}

