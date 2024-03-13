use std::collections::HashMap;

use gc::{Finalize, Gc, GcCell, Trace};

use crate::{ast::Ident, evaluator::EvalError, Expr};

#[derive(Debug, PartialEq, Eq, Clone, Finalize)]
pub enum Value {
    Atom(AtomValue),
    Ref(Gc<RefValue>),
}
unsafe impl Trace for Value {
    gc::custom_trace!(this, {
        match this {
            Value::Atom(_) => {}
            Value::Ref(r) => mark(r),
        }
    });
}
impl Value {
    pub fn int(v: i32) -> Value {
        AtomValue::Int(v).into()
    }
    pub fn intrinsic(id: Ident) -> Value {
        AtomValue::Intrinsic(id).into()
    }
    pub fn null() -> Value {
        AtomValue::Null.into()
    }
    pub fn fun(params: Vec<Ident>, body: Box<Expr>, local_env: Option<LocalEnvRef>) -> Value {
        RefValue::Fun {
            params,
            body,
            local_env,
        }
        .into()
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
    Intrinsic(Ident),
}
impl From<AtomValue> for Value {
    fn from(value: AtomValue) -> Self {
        Value::Atom(value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Finalize)]
struct ValueMap(HashMap<Ident, Value>);
unsafe impl Trace for ValueMap {
    gc::custom_trace!(this, {
        for (_, v) in this.0.iter() {
            mark(v);
        }
    });
}

#[derive(Debug, PartialEq, Eq, Clone, Finalize)]
pub struct LocalEnv {
    values: GcCell<ValueMap>,
    parent: Option<LocalEnvRef>,
}

unsafe impl Trace for LocalEnv {
    gc::custom_trace!(this, {
        mark(&this.values);
        mark(&this.parent);
    });
}

pub type LocalEnvRef = Gc<LocalEnv>;
impl LocalEnv {
    pub fn extend(parent: Option<LocalEnvRef>) -> LocalEnvRef {
        let e = LocalEnv {
            values: GcCell::new(ValueMap(HashMap::new())),
            parent,
        };
        Gc::new(e)
    }

    pub fn bind(local_env: &LocalEnvRef, name: Ident, value: Value) -> Result<(), EvalError> {
        let mut values = local_env.values.borrow_mut();
        if values.0.contains_key(&name) {
            return Err(EvalError::NameDefined(name));
        }
        values.0.insert(name, value);
        Ok(())
    }

    pub fn get_var(local_env: &Option<LocalEnvRef>, name: &Ident) -> Option<Value> {
        let Some(local_env) = local_env else {
            return None;
        };
        let found = local_env.values.borrow().0.get(name).cloned();
        if found.is_some() {
            return found;
        }
        Self::get_var(&local_env.parent, name)
    }

    pub fn reassign_if_exists(
        local_env: &Option<LocalEnvRef>,
        name: &Ident,
        value: Value,
    ) -> Result<(), Value> {
        let Some(local_env) = local_env else {
            return Err(value);
        };
        let mut values = local_env.values.borrow_mut();
        if let Some(v) = values.0.get_mut(name) {
            *v = value;
            return Ok(());
        }
        Self::reassign_if_exists(&local_env.parent, name, value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Finalize)]
pub enum RefValue {
    Fun {
        params: Vec<Ident>,
        body: Box<Expr>,
        local_env: Option<LocalEnvRef>,
    },
}
impl From<RefValue> for Value {
    fn from(value: RefValue) -> Self {
        Value::Ref(Gc::new(value))
    }
}

unsafe impl Trace for RefValue {
    gc::custom_trace!(this, {
        match this {
            RefValue::Fun {
                params: _,
                body: _,
                local_env,
            } => {
                mark(local_env);
            }
        }
    });
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Atom(AtomValue::Int(value))
    }
}
