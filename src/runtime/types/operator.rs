// High level operator defs used by the type system

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Unary(UnaryOp),
    Binary(BinaryOp),
}


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Pos, Neg, Inv, Not,
}


// Binary Operators

// Coercion rules
// arithmetic operations - short-circuit for int/float, coerce to float if either operand is float
// bitwise operations    - short-circuit for bool/int, coerce to int if either operand is int
// shift operations      - short-circuit if LHS is bool/int and RHS is int, result is always int
// inequality comparison - short-circuit for int/flot, result is always bool
// equality              - handled specially, result is always bool

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Arithmetic {
    Mul, Div, Mod,
    Add, Sub,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Bitwise {
    And, Xor, Or,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Shift {
    Left, Right,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Comparison {
    LT, GT, LE, GE, EQ, NE,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Logical {
    And, Or,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Arithmetic(Arithmetic),
    Bitwise(Bitwise),
    Shift(Shift),
    Comparison(Comparison),
    Logical(Logical),
}

impl From<Arithmetic> for BinaryOp {
    fn from(op: Arithmetic) -> Self { Self::Arithmetic(op) }
}

impl From<Bitwise> for BinaryOp {
    fn from(op: Bitwise) -> Self { Self::Bitwise(op) }
}

impl From<Shift> for BinaryOp {
    fn from(op: Shift) -> Self { Self::Shift(op) }
}

impl From<Comparison> for BinaryOp {
    fn from(op: Comparison) -> Self { Self::Comparison(op) }
}

impl From<Logical> for BinaryOp {
    fn from(op: Logical) -> Self { Self::Logical(op) }
}
