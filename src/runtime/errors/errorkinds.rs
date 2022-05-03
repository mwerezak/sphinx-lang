//! Error constructor functions

use crate::utils;
use crate::runtime::Variant;
use crate::runtime::function::Signature;
use crate::runtime::types::MethodTag;
use crate::runtime::strings::{StringValue, StringSymbol, static_symbol};
use crate::runtime::errors::RuntimeError;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    InvalidUnaryOperand,
    InvalidBinaryOperand,
    OverflowError,
    DivideByZero,
    NegativeShiftCount,
    NameNotDefined,
    CantAssignImmutable,
    UnhashableValue,
    MissingArguments,
    TooManyArguments,
    MethodNotSupported,
    AssertFailed,
    InvalidValue,
    Unspecified,
}

impl ErrorKind {
    pub fn name(&self) -> StringValue {
        let name = match self {
            ErrorKind::InvalidUnaryOperand => static_symbol!("InvalidUnaryOperandError"),
            ErrorKind::InvalidBinaryOperand => static_symbol!("InvalidBinaryOperandError"),
            ErrorKind::OverflowError => static_symbol!("OverflowError"),
            ErrorKind::DivideByZero => static_symbol!("DivideByZeroError"),
            ErrorKind::NegativeShiftCount => static_symbol!("NegativeShiftCountError"),
            ErrorKind::NameNotDefined => static_symbol!("NameNotDefinedError"),
            ErrorKind::CantAssignImmutable => static_symbol!("CantAssignImmutableError"),
            ErrorKind::UnhashableValue => static_symbol!("UnhashableValueError"),
            ErrorKind::MissingArguments => static_symbol!("MissingArgumentsError"),
            ErrorKind::TooManyArguments => static_symbol!("TooManyArgumentsError"),
            ErrorKind::MethodNotSupported => static_symbol!("MethodNotSupportedError"),
            ErrorKind::AssertFailed => static_symbol!("AssertFailedError"),
            ErrorKind::InvalidValue => static_symbol!("InvalidValueError"),
            ErrorKind::Unspecified => static_symbol!("UnspecifiedError"),
        };
        name.into()
    }
}



fn format_type(value: &Variant) -> StringValue {
    value.type_name().unwrap_or_else(
        |_| value.type_tag().name(),
    )
}

impl RuntimeError {
    pub fn invalid_unary_operand(operand: &Variant) -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::InvalidUnaryOperand,
            StringValue::new_uninterned(format!(
                "unsupported operand: '{}'", format_type(operand)
            )),
        ))
    }

    pub fn invalid_binary_operands(lhs: &Variant, rhs: &Variant) -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::InvalidUnaryOperand,
            StringValue::new_uninterned(format!(
                "unsupported operands: '{}' and '{}'", 
                format_type(lhs), format_type(rhs)
            )),
        ))
    }

    pub fn overflow_error() -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::OverflowError,
            static_symbol!("integer overflow").into(),
        ))
    }

    pub fn divide_by_zero() -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::DivideByZero,
            static_symbol!("divide by zero").into(),
        ))
    }

    pub fn negative_shift_count() -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::NegativeShiftCount,
            static_symbol!("negative bitshift count").into(),
        ))
    }

    pub fn name_not_defined(name: StringSymbol) -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::NameNotDefined,
            StringValue::new_uninterned(format!("undefined variable \"{}\"", name)),
        ))
    }

    pub fn cant_assign_immutable(name: StringSymbol) -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::CantAssignImmutable,
            StringValue::new_uninterned(format!("can't assign to immutable name \"{}\"", name)),
        ))
    }

    pub fn unhashable_value(value: &Variant) -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::UnhashableValue,
            StringValue::new_uninterned(format!("{} is not hashable", value.display_echo())),
        ))
    }

    pub fn assert_failed(message: Option<StringValue>) -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::AssertFailed,
            match message {
                Some(message) => StringValue::new_uninterned(format!("assertion failed: {}", message)),
                None => static_symbol!("assertion failed").into(),
            },
        ))
    }

    pub fn missing_arguments(signature: &Signature, nargs: usize) -> Box<Self> {
        let missing = signature.required().iter()
            .skip(nargs)
            .map(|param| *param.name())
            .collect::<Vec<StringSymbol>>();
        
        let count = signature.min_arity() - nargs;
        
        let message = format!(
            "{} missing {} required {}: {}",
            signature.fmt_name(), 
            count, 
            if count == 1 { "argument" }
            else { "arguments" },
            utils::fmt_join(", ", &missing),
        );
        
        Box::new(Self::new(
            ErrorKind::MissingArguments,
            StringValue::new_uninterned(message),
        ))
    }

    pub fn too_many_arguments(signature: &Signature, nargs: usize) -> Box<Self> {
        let message = format!(
            "{} takes {} arguments but {} were given", 
            signature.fmt_name(), 
            signature.max_arity().unwrap(), 
            nargs,
        );
        
        Box::new(Self::new(
            ErrorKind::TooManyArguments,
            StringValue::new_uninterned(message),
        ))
    }

    pub fn metamethod_not_supported(receiver: &Variant, method: MethodTag) -> Box<Self> {
        let receiver = format_type(receiver);
        
        let message = match method {
            MethodTag::AsBits => format!("can't interpret '{}' as bitfield", receiver),
            MethodTag::AsInt => format!("can't interpret '{}' as int", receiver),
            MethodTag::AsFloat => format!("can't interpret '{}' as float", receiver),
            MethodTag::Invoke => format!("type '{}' is not callable", receiver),
            
            MethodTag::IterInit => format!("type '{}' is not iterable", receiver),
            MethodTag::IterNext | MethodTag::IterItem
                => format!("type '{}' is not an iterator", receiver),
            
            _ => format!("type '{}' does not support '__{}'", receiver, method),
        };
        
        Box::new(Self::new(
            ErrorKind::MethodNotSupported,
            StringValue::new_uninterned(message),
        ))
    }

    pub fn invalid_value(message: impl AsRef<str>) -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::InvalidValue,
            StringValue::new_uninterned(message.as_ref()),
        ))
    }

    pub fn other(message: impl AsRef<str>) -> Box<Self> {
        Box::new(Self::new(
            ErrorKind::Unspecified,
            StringValue::new_uninterned(message.as_ref()),
        ))
    }
}