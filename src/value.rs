use std::{collections::HashMap, fmt::Display};

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
        v.into()
    }
    pub fn bool(v: bool) -> Value {
        v.into()
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
    pub fn object(obj: ObjectValue) -> Value {
        RefValue::Object(GcCell::new(obj)).into()
    }
    pub fn array(v: Vec<Value>) -> Value {
        RefValue::Array(GcCell::new(v)).into()
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Ref(ref_value) => match &**ref_value {
                RefValue::Array(values) => {
                    f.write_str("[")?;
                    let values = values.borrow();
                    let mut iter = values.iter();
                    if let Some(v) = iter.next() {
                        v.fmt(f)?;
                        for v in iter {
                            f.write_str(", ")?;
                            v.fmt(f)?;
                        }
                    }
                    f.write_str("]")?;
                }
                RefValue::Object(obj) => {
                    obj.borrow().fmt(f)?;
                }
                RefValue::Fun { .. } => {
                    f.write_str("#fun")?;
                }
            },
            Value::Atom(atom_value) => match atom_value {
                AtomValue::Bool(v) => {
                    v.fmt(f)?;
                }
                AtomValue::Null => {
                    f.write_str("null")?;
                }
                AtomValue::Int(v) => {
                    v.fmt(f)?;
                }
                AtomValue::Intrinsic(name) => {
                    f.write_str("#fun:")?;
                    name.fmt(f)?;
                }
            },
        }
        Ok(())
    }
}

impl TryFrom<&Value> for i32 {
    type Error = EvalError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Atom(AtomValue::Int(v)) => Ok(*v),
            _ => Err(EvalError::TypeError("Int".into(), value.clone())),
        }
    }
}
impl TryFrom<&Value> for bool {
    type Error = EvalError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Atom(AtomValue::Bool(v)) => Ok(*v),
            _ => Err(EvalError::TypeError("Bool".into(), value.clone())),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AtomValue {
    Null,
    Int(i32),
    Intrinsic(Ident),
    Bool(bool),
}
impl From<AtomValue> for Value {
    fn from(value: AtomValue) -> Self {
        Value::Atom(value)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Atom(AtomValue::Int(value))
    }
}
impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Atom(AtomValue::Bool(value))
    }
}
impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(value: Vec<T>) -> Self {
        Value::array(value.into_iter().map(|v| v.into()).collect())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Finalize)]
pub struct ValueMap(pub HashMap<Ident, Value>);
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
    Object(GcCell<ObjectValue>),
    Array(GcCell<Vec<Value>>),
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
            RefValue::Object(o) => {
                mark(o);
            }
            RefValue::Array(vs) => {
                mark(vs);
            }
        }
    });
}

#[derive(Debug, PartialEq, Eq, Clone, Finalize)]
pub struct ObjectValue {
    names: Vec<Ident>,
    values: HashMap<Ident, Value>,
}
impl Default for ObjectValue {
    fn default() -> Self {
        Self::new()
    }
}

impl ObjectValue {
    pub fn new() -> ObjectValue {
        ObjectValue {
            names: Vec::new(),
            values: HashMap::new(),
        }
    }
    pub fn insert(&mut self, name: Ident, value: Value) {
        let old = self.values.insert(name.clone(), value);
        if old.is_none() {
            self.names.push(name);
        }
    }
    pub fn get(&self, name: &Ident) -> Option<&Value> {
        self.values.get(name)
    }
}
unsafe impl Trace for ObjectValue {
    gc::custom_trace!(this, {
        for v in this.values.values() {
            mark(v);
        }
    });
}
impl Display for ObjectValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("{")?;
        let mut iter = self.names.iter();
        if let Some(name) = iter.next() {
            name.fmt(f)?;
            f.write_str(": ")?;
            self.values.get(name).unwrap().fmt(f)?;
            for name in iter {
                f.write_str(", ")?;
                name.fmt(f)?;
                f.write_str(": ")?;
                self.values.get(name).unwrap().fmt(f)?;
            }
        }
        f.write_str("}")?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::{array_value, object_value};

    use super::*;

    macro_rules! assert_display {
        ($expr:expr, $expected:literal) => {
            assert_eq!(&format!("{}", Into::<Value>::into($expr)), $expected);
        };
    }

    #[test]
    fn display_int() {
        assert_display!(1, "1");
    }

    #[test]
    fn dislpay_array() {
        assert_display!(array_value![], "[]");
        assert_display!(array_value![1, 2, 3], "[1, 2, 3]");
    }

    #[test]
    fn display_object() {
        assert_display!(object_value! {}, "{}");
        assert_display!(
            object_value! {foo: 1, bar: object_value!{}},
            "{foo: 1, bar: {}}"
        );
    }

    #[test]
    fn display_fun() {
        assert_display!(
            Into::<Value>::into(RefValue::Fun {
                params: vec![],
                body: Box::new(Expr::Var("".into())),
                local_env: None
            }),
            "#fun"
        );
    }

    #[test]
    fn display_null() {
        assert_display!(Value::null(), "null");
    }

    #[test]
    fn display_bool() {
        assert_display!(true, "true");
        assert_display!(false, "false");
    }

    #[test]
    fn display_intrinsic() {
        assert_display!(AtomValue::Intrinsic("foo".into()), "#fun:foo");
    }
}
