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
pub struct LocalEnv {
    values: HashMap<Ident, Value>,
    parent: Option<LocalEnvRef>,
}

unsafe impl Trace for LocalEnv {
    gc::custom_trace!(this, {
        mark(&this.parent);
        for v in this.values.values() {
            mark(v);
        }
    });
}

pub type LocalEnvRef = Gc<GcCell<LocalEnv>>;
impl LocalEnv {
    pub fn extend(parent: Option<LocalEnvRef>) -> LocalEnvRef {
        let e = LocalEnv {
            values: HashMap::new(),
            parent,
        };
        Gc::new(GcCell::new(e))
    }

    pub fn bind<S: Into<Ident>>(local_env: &LocalEnvRef, name: S, value: Value) {
        (*local_env).borrow_mut().values.insert(name.into(), value);
    }

    pub fn get_var(local_env: &Option<LocalEnvRef>, name: &Ident) -> Option<Value> {
        if let Some(local_env) = local_env {
            let v = (*local_env).borrow_mut().values.get(name).cloned();
            v.or_else(|| LocalEnv::get_var(&(*local_env).borrow().parent, name))
        } else {
            None
        }
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
