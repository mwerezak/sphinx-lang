//! Error constructor functions

use crate::utils;
use crate::runtime::Variant;
use crate::runtime::function::Signature;
use crate::runtime::types::MethodTag;
use crate::runtime::strings::{StringValue, StringSymbol};
use crate::runtime::errors::{RuntimeError, ErrorKind};


fn format_type(value: &Variant) -> StringValue {
    value.type_name().unwrap_or_else(
        |_| StringValue::new_maybe_interned(value.type_tag().name()),
    )
}

impl RuntimeError {
    pub fn invalid_unary_operand(operand: &Variant) -> Box<RuntimeError> {
        Self::new(
            ErrorKind::InvalidUnaryOperand,
            StringValue::new_uninterned(format!(
                "unsupported operand: '{}'", format_type(operand)
            )),
        )
    }

    pub fn invalid_binary_operands(lhs: &Variant, rhs: &Variant) -> Box<RuntimeError> {
        RuntimeError::new(
            ErrorKind::InvalidUnaryOperand,
            StringValue::new_uninterned(format!(
                "unsupported operands: '{}' and '{}'", 
                format_type(lhs), format_type(rhs)
            )),
        )
    }

    pub fn overflow_error() -> Box<RuntimeError> {
        RuntimeError::new(
            ErrorKind::OverflowError,
            StringValue::new_maybe_interned("integer overflow"),
        )
    }

    pub fn divide_by_zero() -> Box<RuntimeError> {
        RuntimeError::new(
            ErrorKind::DivideByZero,
            StringValue::new_maybe_interned("divide by zero"),
        )
    }

    pub fn negative_shift_count() -> Box<RuntimeError> {
        RuntimeError::new(
            ErrorKind::NegativeShiftCount,
            StringValue::new_maybe_interned("negative bitshift count"),
        )
    }

    pub fn name_not_defined(name: StringSymbol) -> Box<RuntimeError> {
        RuntimeError::new(
            ErrorKind::NameNotDefined,
            StringValue::new_uninterned(format!("undefined variable \"{}\"", name)),
        )
    }

    pub fn cant_assign_immutable(name: StringSymbol) -> Box<RuntimeError> {
        RuntimeError::new(
            ErrorKind::CantAssignImmutable,
            StringValue::new_uninterned(format!("can't assign to immutable name \"{}\"", name)),
        )
    }

    pub fn unhashable_value(value: &Variant) -> Box<RuntimeError> {
        RuntimeError::new(
            ErrorKind::UnhashableValue,
            StringValue::new_uninterned(format!("{} is not hashable", value.display_echo())),
        )
    }

    pub fn assert_failed(message: Option<StringValue>) -> Box<RuntimeError> {
        RuntimeError::new(
            ErrorKind::AssertFailed,
            match message {
                Some(message) => StringValue::new_uninterned(format!("assertion failed: {}", message)),
                None => StringValue::new_maybe_interned("assertion failed"),
            },
        )
    }

    pub fn missing_arguments(signature: &Signature, nargs: usize) -> Box<RuntimeError> {
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
        
        RuntimeError::new(
            ErrorKind::MissingArguments,
            StringValue::new_uninterned(message),
        )
    }

    pub fn too_many_arguments(signature: &Signature, nargs: usize) -> Box<RuntimeError> {
        let message = format!(
            "{} takes {} arguments but {} were given", 
            signature.fmt_name(), 
            signature.max_arity().unwrap(), 
            nargs,
        );
        
        RuntimeError::new(
            ErrorKind::TooManyArguments,
            StringValue::new_uninterned(message),
        )
    }

    pub fn metamethod_not_supported(receiver: &Variant, method: MethodTag) -> Box<RuntimeError> {
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
        
        RuntimeError::new(
            ErrorKind::MethodNotSupported,
            StringValue::new_uninterned(message),
        )
    }

    pub fn invalid_value(message: impl AsRef<str>) -> Box<RuntimeError> {
        RuntimeError::new(
            ErrorKind::InvalidValue,
            StringValue::new_uninterned(message.as_ref()),
        )
    }

    pub fn other(message: impl AsRef<str>) -> Box<RuntimeError> {
        RuntimeError::new(
            ErrorKind::Unspecified,
            StringValue::new_uninterned(message.as_ref()),
        )
    }
}