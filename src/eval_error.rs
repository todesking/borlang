use crate::ast::Ident;

use crate::Value;

use std::error::Error;
use std::fmt::Debug;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq)]
pub enum EvalError {
    TypeError(String, Value),
    NameNotFound(Ident),
    NameDefined(Ident),
    PropertyNotFound(Ident),
    ArgumentLength { expected: usize, actual: usize },
    IndexOutOfBound { len: usize, index: usize },
    AssignNotSupported,
    CastError(String, Value),
    TraitProtocol(String),
    NumericRange,
}
impl EvalError {
    pub fn name_not_found<S: Into<Ident>>(name: S) -> Self {
        Self::NameNotFound(name.into())
    }
    pub fn type_error<S: Into<String>, V: Into<Value>>(name: S, value: V) -> Self {
        Self::TypeError(name.into(), value.into())
    }
    pub fn cast_error<S: Into<String>, V: Into<Value>>(name: S, value: V) -> Self {
        Self::CastError(name.into(), value.into())
    }

    pub fn trait_protocol<S: Into<String>>(msg: S) -> EvalError {
        Self::TraitProtocol(msg.into())
    }

    pub fn argument_length(expected: usize, actual: usize) -> EvalError {
        Self::ArgumentLength { expected, actual }
    }

    pub fn property_not_found<S: Into<String>>(name: S) -> Self {
        EvalError::PropertyNotFound(name.into().into())
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
impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
impl Error for EvalError {}
