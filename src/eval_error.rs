use crate::compiler::CompileError;
use crate::module::LoadError;

use crate::value::ObjectKey;
use crate::Value;

use std::fmt::Debug;

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum EvalError {
    #[error("Type error: expected {0}, actual={1}")]
    TypeError(String, Value),
    #[error("Name not found: {0}")]
    NameNotFound(String),
    #[error("Already defined: {0}")]
    NameDefined(String),
    #[error("Property not found: {0}")]
    PropertyNotFound(ObjectKey),
    #[error("Invalid argument length: expected={expected}, actual={actual}")]
    ArgumentLength { expected: usize, actual: usize },
    #[error("Invalid array length: expected={expected}, actual={actual}")]
    ArrayLength { expected: usize, actual: usize },
    #[error("Index out of bound: len={len}, index={index}")]
    IndexOutOfBound { len: usize, index: usize },
    #[error("Can't assign")]
    AssignNotSupported,
    #[error("Can't cast to {0}: {1}")]
    CastError(String, Value),
    #[error("Numeric range error")]
    NumericRange,
    #[error(transparent)]
    LoadError(LoadError),
    #[error("Exception: data={data}")]
    Exception { data: Value },
    #[error("Compile error: {0:?}")]
    CompileError(CompileError),
}
impl EvalError {
    pub fn name_not_found<S: Into<String>>(name: S) -> Self {
        Self::NameNotFound(name.into())
    }
    pub fn type_error<S: Into<String>, V: Into<Value>>(name: S, value: V) -> Self {
        Self::TypeError(name.into(), value.into())
    }
    pub fn cast_error<S: Into<String>, V: Into<Value>>(name: S, value: V) -> Self {
        Self::CastError(name.into(), value.into())
    }

    pub fn argument_length(expected: usize, actual: usize) -> EvalError {
        Self::ArgumentLength { expected, actual }
    }

    pub fn property_not_found(key: ObjectKey) -> Self {
        EvalError::PropertyNotFound(key)
    }

    pub fn array_length(expected: usize, actual: usize) -> Self {
        EvalError::ArrayLength { expected, actual }
    }

    pub fn check_argument_len(expected: usize, actual: usize) -> Result<(), EvalError> {
        if expected != actual {
            return Err(EvalError::argument_length(expected, actual));
        }
        Ok(())
    }

    pub fn check_array_index(len: usize, index: usize) -> Result<(), EvalError> {
        if len <= index {
            return Err(Self::IndexOutOfBound { len, index });
        }
        Ok(())
    }
}
