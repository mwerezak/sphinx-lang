use crate::language::{IntType, FloatType};
use crate::runtime::Variant;
use crate::runtime::gc::GCTrace;
use crate::runtime::strings::StringSymbol;
use crate::runtime::function::Call;
use crate::runtime::errors::ExecResult;


type Method0<T, R> = fn(T) -> ExecResult<R>;
type Method1<T, R> = fn(T, &Variant) -> ExecResult<R>;
// type Method2<R> = fn(&Variant, &Variant, &Variant) -> ExecResult<R>;

pub type MethodUnary<T> = Method0<T, Variant>;
pub type MethodBinary<T> = Method1<T, Option<Variant>>; // a None result will cause the reflected version to be called
pub type MethodBinaryReflected<T> = Method1<T, Variant>;
pub type MethodCompare<T> = Method1<T, Option<bool>>;

pub type MethodCall<T> = fn(T, &[Variant]) -> Call;

pub struct Metatable<T> {
    call: Option<MethodCall<T>>,
    
    // Primitive coercions
    to_bits: Option<Method0<T, IntType>>,   // __bits
    to_int: Option<Method0<T, IntType>>,    // __int
    to_float: Option<Method0<T, FloatType>>,  // __float
    // to_string: Option<Method0<String>>,  // __tostring
    
    // Operator Overloads
    op_pos: Option<MethodUnary<T>>,  // __pos
    op_neg: Option<MethodUnary<T>>,  // __neg
    op_inv: Option<MethodUnary<T>>,  // __inv
    
    op_add: Option<MethodBinary<T>>, // __add
    op_sub: Option<MethodBinary<T>>, // __sub
    op_mul: Option<MethodBinary<T>>, // __mul
    op_div: Option<MethodBinary<T>>, // __div
    op_mod: Option<MethodBinary<T>>, // __mod
    op_and: Option<MethodBinary<T>>, // __and
    op_xor: Option<MethodBinary<T>>, // __xor
    op_or:  Option<MethodBinary<T>>, // __or
    op_shl: Option<MethodBinary<T>>, // __shl
    op_shr: Option<MethodBinary<T>>, // __shr
    
    op_radd: Option<MethodBinaryReflected<T>>, // __radd
    op_rsub: Option<MethodBinaryReflected<T>>, // __rsub
    op_rmul: Option<MethodBinaryReflected<T>>, // __rmul
    op_rdiv: Option<MethodBinaryReflected<T>>, // __rdiv
    op_rmod: Option<MethodBinaryReflected<T>>, // __rmod
    op_rand: Option<MethodBinaryReflected<T>>, // __rand
    op_rxor: Option<MethodBinaryReflected<T>>, // __rxor
    op_ror:  Option<MethodBinaryReflected<T>>, // __ror
    op_rshl: Option<MethodBinaryReflected<T>>, // __rshl
    op_rshr: Option<MethodBinaryReflected<T>>, // __rshr
    
    cmp_lt: Option<MethodCompare<T>>, // __lt
    cmp_le: Option<MethodCompare<T>>, // __le
    cmp_eq: Option<MethodCompare<T>>, // __eq
}

unsafe impl<T> GCTrace for Metatable<T> {
    fn trace(&self) {
        // don't need to do anything as the metatable only holds function pointers
    }
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

impl<T> Metatable<T> {
    #[inline]
    pub fn op_unary(&self, tag: UnaryTag) -> Option<&MethodUnary<T>> {
        match tag {
            UnaryTag::Pos => self.op_pos.as_ref(),
            UnaryTag::Neg => self.op_neg.as_ref(),
            UnaryTag::Inv => self.op_inv.as_ref(),
        }
    }
    
    pub fn set_unary(&mut self, tag: UnaryTag, method: MethodUnary<T>) -> Option<MethodUnary<T>> {
        match tag {
            UnaryTag::Pos => self.op_pos.replace(method),
            UnaryTag::Neg => self.op_neg.replace(method),
            UnaryTag::Inv => self.op_inv.replace(method),
        }
    }
    
    pub fn take_unary(&mut self, tag: UnaryTag) -> Option<MethodUnary<T>> {
        match tag {
            UnaryTag::Pos => self.op_pos.take(),
            UnaryTag::Neg => self.op_neg.take(),
            UnaryTag::Inv => self.op_inv.take(),
        }
    }
    
    #[inline]
    pub fn op_binary(&self, tag: BinaryTag) -> Option<&MethodBinary<T>> {
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
    
    pub fn set_binary(&mut self, tag: BinaryTag, method: MethodBinary<T>) -> Option<MethodBinary<T>> {
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
    
    pub fn take_binary(&mut self, tag: BinaryTag) -> Option<MethodBinary<T>> {
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
    pub fn op_binary_reflected(&self, tag: BinaryTag) -> Option<&MethodBinaryReflected<T>> {
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
    
    pub fn set_binary_reflected(&mut self, tag: BinaryTag, method: MethodBinaryReflected<T>) -> Option<MethodBinaryReflected<T>> {
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
    
    pub fn take_binary_reflected(&mut self, tag: BinaryTag) -> Option<MethodBinaryReflected<T>> {
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
    pub fn op_compare(&self, tag: CompareTag) -> Option<&MethodCompare<T>> {
        match tag {
            CompareTag::LT => self.cmp_lt.as_ref(),
            CompareTag::LE => self.cmp_le.as_ref(),
            CompareTag::EQ => self.cmp_eq.as_ref(),
        }
    }
    
    pub fn set_compare(&mut self, tag: CompareTag, method: MethodCompare<T>) -> Option<MethodCompare<T>> {
        match tag {
            CompareTag::LT => self.cmp_lt.replace(method),
            CompareTag::LE => self.cmp_le.replace(method),
            CompareTag::EQ => self.cmp_eq.replace(method),
        }
    }
    
    pub fn take_compare(&mut self, tag: CompareTag) -> Option<MethodCompare<T>> {
        match tag {
            CompareTag::LT => self.cmp_lt.take(),
            CompareTag::LE => self.cmp_le.take(),
            CompareTag::EQ => self.cmp_eq.take(),
        }
    }
}

impl<T> Default for Metatable<T> {
    fn default() -> Self {
        Self {
            call: None,
            
            to_bits: None,
            to_int: None,
            to_float: None,
            
            op_pos: None,
            op_neg: None,
            op_inv: None,
            
            op_add: None,
            op_sub: None,
            op_mul: None,
            op_div: None,
            op_mod: None,
            
            op_and: None,
            op_xor: None,
            op_or: None,
            op_shl: None,
            op_shr: None,
            
            op_radd: None,
            op_rsub: None,
            op_rmul: None,
            op_rdiv: None,
            
            op_rmod: None,
            op_rand: None,
            op_rxor: None,
            op_ror: None,
            op_rshl: None,
            op_rshr: None,
            
            cmp_lt: None,
            cmp_le: None,
            cmp_eq: None,
        }
    }
}