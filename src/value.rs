use crate::{ast::Ident, Expr};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Null,
    Int(i32),
    Intrinsic(String),
    RefValue(RefValue),
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RefValue {
    Fun { params: Vec<Ident>, body: Box<Expr> },
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Int(value)
    }
}
