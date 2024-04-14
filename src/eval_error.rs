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
    IndexOutOfBound { len: usize, index: i32 },
    AssignNotSupported,
}
impl EvalError {
    pub fn name_not_found<S: Into<Ident>>(name: S) -> Self {
        Self::NameNotFound(name.into())
    }
    pub fn type_error<S: Into<String>, V: Into<Value>>(name: S, value: V) -> Self {
        Self::TypeError(name.into(), value.into())
    }
}
impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
impl Error for EvalError {}
