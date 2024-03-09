use gc::{Finalize, Gc, Trace};

use crate::{ast::Ident, evaluator::EvalError, Expr};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Atom(AtomValue),
    Ref(Gc<RefValue>),
}
impl Value {
    pub fn int(v: i32) -> Value {
        AtomValue::Int(v).into()
    }
    pub fn intrinsic(id: String) -> Value {
        AtomValue::Intrinsic(id).into()
    }
    pub fn null() -> Value {
        AtomValue::Null.into()
    }
    pub fn fun(params: Vec<Ident>, body: Box<Expr>) -> Value {
        RefValue::Fun { params, body }.into()
    }
}
impl TryInto<i32> for Value {
    type Error = EvalError;

    fn try_into(self) -> Result<i32, Self::Error> {
        match self {
            Value::Atom(AtomValue::Int(v)) => Ok(v),
            _ => Err(EvalError::TypeError("Int".into(), self.clone())),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AtomValue {
    Null,
    Int(i32),
    Intrinsic(String),
}
impl From<AtomValue> for Value {
    fn from(value: AtomValue) -> Self {
        Value::Atom(value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Finalize)]
pub enum RefValue {
    Fun { params: Vec<Ident>, body: Box<Expr> },
}
impl From<RefValue> for Value {
    fn from(value: RefValue) -> Self {
        Value::Ref(Gc::new(value))
    }
}

unsafe impl Trace for RefValue {
    gc::custom_trace!(this, {
        match this {
            RefValue::Fun { params: _, body: _ } => {}
        }
    });
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Atom(AtomValue::Int(value))
    }
}
