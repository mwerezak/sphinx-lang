use crate::runtime::Variant;
use crate::runtime::ops::eval_not;
use crate::runtime::errors::{ExecResult, RuntimeError, ErrorKind};


type Method0<R> = fn(&Variant) -> ExecResult<R>;
type Method1<R> = fn(&Variant, &Variant) -> ExecResult<R>;

pub type MethodSelf = Method0<Variant>;
pub type MethodUnary = Method0<Variant>;
pub type MethodBinary = Method1<Option<Variant>>; // a None result will cause the reflected version to be called
pub type MethodBinaryReflected = Method1<Variant>;
pub type MethodCompare = Method1<Option<bool>>;

#[derive(Default)]
pub struct Metatable {
    tostring: Option<MethodSelf>,
    
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
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum MethodTag {
    ToString,
}

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

// Slot Selector Helpers

macro_rules! select_method {
    ($self:expr, ref $tag:expr ) => {
        match $tag {
            MethodTag::ToString => $self.tostring.as_ref(),
        }
    };
    ($self:expr, mut $tag:expr ) => {
        &mut match $tag {
            MethodTag::ToString => $self.tostring,
        } 
    };
}

macro_rules! select_unary {
    ($self:expr, ref $tag:expr ) => {
        match $tag {
            UnaryTag::Pos => $self.op_pos.as_ref(),
            UnaryTag::Neg => $self.op_neg.as_ref(),
            UnaryTag::Inv => $self.op_inv.as_ref(),
        }
    };
    ($self:expr, mut $tag:expr ) => {
        &mut match $tag {
            UnaryTag::Pos => $self.op_pos,
            UnaryTag::Neg => $self.op_neg,
            UnaryTag::Inv => $self.op_inv,
        }
    };
}

macro_rules! select_binary {
    ( $self:expr, ref $tag:expr ) => {
        match $tag {
            BinaryTag::Add => $self.op_add.as_ref(),
            BinaryTag::Sub => $self.op_sub.as_ref(),
            BinaryTag::Mul => $self.op_mul.as_ref(),
            BinaryTag::Div => $self.op_div.as_ref(),
            BinaryTag::Mod => $self.op_mod.as_ref(),
            BinaryTag::And => $self.op_and.as_ref(),
            BinaryTag::Xor => $self.op_xor.as_ref(),
            BinaryTag::Or  => $self.op_or.as_ref(),
            BinaryTag::Shl => $self.op_shl.as_ref(),
            BinaryTag::Shr => $self.op_shr.as_ref(),
        }
    };
    ( $self:expr, mut $tag:expr ) => {
        &mut match $tag {
            BinaryTag::Add => $self.op_add,
            BinaryTag::Sub => $self.op_sub,
            BinaryTag::Mul => $self.op_mul,
            BinaryTag::Div => $self.op_div,
            BinaryTag::Mod => $self.op_mod,
            BinaryTag::And => $self.op_and,
            BinaryTag::Xor => $self.op_xor,
            BinaryTag::Or  => $self.op_or,
            BinaryTag::Shl => $self.op_shl,
            BinaryTag::Shr => $self.op_shr,
        }
    };
}

macro_rules! select_binary_reflected {
    ( $self:expr, ref $tag:expr ) => {
        match $tag {
            BinaryTag::Add => $self.op_radd.as_ref(),
            BinaryTag::Sub => $self.op_rsub.as_ref(),
            BinaryTag::Mul => $self.op_rmul.as_ref(),
            BinaryTag::Div => $self.op_rdiv.as_ref(),
            BinaryTag::Mod => $self.op_rmod.as_ref(),
            BinaryTag::And => $self.op_rand.as_ref(),
            BinaryTag::Xor => $self.op_rxor.as_ref(),
            BinaryTag::Or  => $self.op_ror.as_ref(),
            BinaryTag::Shl => $self.op_rshl.as_ref(),
            BinaryTag::Shr => $self.op_rshr.as_ref(),
        }
    };
    ( $self:expr, mut $tag:expr ) => {
        &mut match $tag {
            BinaryTag::Add => $self.op_radd,
            BinaryTag::Sub => $self.op_rsub,
            BinaryTag::Mul => $self.op_rmul,
            BinaryTag::Div => $self.op_rdiv,
            BinaryTag::Mod => $self.op_rmod,
            BinaryTag::And => $self.op_rand,
            BinaryTag::Xor => $self.op_rxor,
            BinaryTag::Or  => $self.op_ror,
            BinaryTag::Shl => $self.op_rshl,
            BinaryTag::Shr => $self.op_rshr,
        }
    };
}

macro_rules! select_comparison {
    ( $self:expr, ref $tag:expr ) => {
        match $tag {
            CompareTag::LT => $self.cmp_lt.as_ref(),
            CompareTag::LE => $self.cmp_le.as_ref(),
            CompareTag::EQ => $self.cmp_eq.as_ref(),
        }
    };
    ( $self:expr, mut $tag:expr ) => {
        &mut match $tag {
            CompareTag::LT => $self.cmp_lt,
            CompareTag::LE => $self.cmp_le,
            CompareTag::EQ => $self.cmp_eq,
        }
    };
}

impl Metatable {
    #[inline]
    pub fn method(&self, tag: &MethodTag) -> Option<&MethodSelf> {
        select_method!(&self, ref tag)
    }
    
    pub fn set_method(&mut self, tag: &MethodTag, method: MethodSelf) -> Option<MethodSelf> {
        select_method!(self, mut tag).replace(method)
    }
    
    pub fn take_method(&mut self, tag: &MethodTag) -> Option<MethodSelf> {
        select_method!(self, mut tag).take()
    }
    
    #[inline]
    pub fn op_unary(&self, tag: &UnaryTag) -> Option<&MethodUnary> {
        select_unary!(self, ref tag)
    }
    
    pub fn set_unary(&mut self, tag: &UnaryTag, method: MethodUnary) -> Option<MethodUnary> {
        select_unary!(self, mut tag).replace(method)
    }
    
    pub fn take_unary(&mut self, tag: &UnaryTag) -> Option<MethodUnary> {
        select_unary!(self, mut tag).take()
    }
    
    #[inline]
    pub fn op_binary(&self, tag: &BinaryTag) -> Option<&MethodBinary> {
        select_binary!(self, ref tag)
    }
    
    pub fn set_binary(&mut self, tag: &BinaryTag, method: MethodBinary) -> Option<MethodBinary> {
        select_binary!(self, mut tag).replace(method)
    }
    
    pub fn take_binary(&mut self, tag: &BinaryTag) -> Option<MethodBinary> {
        select_binary!(self, mut tag).take()
    }
    
    #[inline]
    pub fn op_binary_reflected(&self, tag: &BinaryTag) -> Option<&MethodBinaryReflected> {
        select_binary_reflected!(self, ref tag)
    }
    
    pub fn set_binary_reflected(&mut self, tag: &BinaryTag, method: MethodBinaryReflected) -> Option<MethodBinaryReflected> {
        select_binary_reflected!(self, mut tag).replace(method)
    }
    
    pub fn take_binary_reflected(&mut self, tag: &BinaryTag) -> Option<MethodBinaryReflected> {
        select_binary_reflected!(self, mut tag).take()
    }
    
    #[inline]
    pub fn op_compare(&self, tag: &CompareTag) -> Option<&MethodCompare> {
        select_comparison!(self, ref tag)
    }
    
    pub fn set_compare(&mut self, tag: &CompareTag, method: MethodCompare) -> Option<MethodCompare> {
        select_comparison!(self, mut tag).replace(method)
    }
    
    pub fn take_compare(&mut self, tag: &CompareTag) -> Option<MethodCompare> {
        select_comparison!(self, mut tag).take()
    }
}


