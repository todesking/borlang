use crate::{ast::Ident, module::Module, EvalError, EvalResult, Expr};
use gc::{Finalize, Gc, GcCell, Trace};
use std::{collections::HashMap, fmt::Write, rc::Rc};

#[derive(Debug, PartialEq, Eq, Clone, Finalize)]
pub enum Value {
    Null,
    Int(i32),
    Intrinsic(Ident),
    Bool(bool),
    Str(Rc<String>),
    Sym(Rc<String>),
    Ref(RefValue),
}
unsafe impl Trace for Value {
    gc::custom_trace!(this, {
        if let Value::Ref(r) = this {
            mark(r);
        }
    });
}
impl Value {
    pub fn int(v: i32) -> Value {
        v.into()
    }
    pub fn try_int<N: TryInto<i32>>(n: N) -> EvalResult {
        n.try_into()
            .map(Self::int)
            .map_err(|_| EvalError::NumericRange)
    }
    pub fn bool(v: bool) -> Value {
        Value::Bool(v)
    }
    pub fn intrinsic<S: Into<Ident>>(id: S) -> Value {
        Value::Intrinsic(id.into())
    }
    pub fn null() -> Value {
        Value::Null
    }
    pub fn sym<S: Into<String>>(s: S) -> Value {
        Value::Sym(Rc::new(s.into()))
    }
    pub fn fun(
        params: Vec<Ident>,
        body: Box<Expr>,
        local_env: Option<LocalEnvRef>,
        current_module: Gc<Module>,
    ) -> Value {
        RefValue::Fun {
            params,
            body,
            local_env,
            current_module,
        }
        .into()
    }
    pub fn object(obj: ObjectValue) -> Value {
        RefValue::Object(Gc::new(GcCell::new(obj))).into()
    }
    pub fn array(v: Vec<Value>) -> Value {
        RefValue::Array(Gc::new(GcCell::new(v))).into()
    }

    pub fn use_array<T, F: FnOnce(&[Value]) -> EvalResult<T>>(&self, f: F) -> EvalResult<T> {
        match self {
            Value::Ref(RefValue::Array(arr)) => {
                let arr = arr.borrow();
                f(&arr)
            }
            _ => Err(EvalError::type_error("Array", self.clone())),
        }
    }
    pub fn use_array_mut<T, F: FnOnce(&mut Vec<Value>) -> EvalResult<T>>(
        &self,
        f: F,
    ) -> EvalResult<T> {
        match self {
            Value::Ref(RefValue::Array(arr)) => f(&mut arr.borrow_mut()),
            _ => Err(EvalError::type_error("Array", self.clone())),
        }
    }
    pub fn use_object<T, F: FnOnce(&ObjectValue) -> EvalResult<T>>(&self, f: F) -> EvalResult<T> {
        match self {
            Value::Ref(RefValue::Object(obj)) => f(&obj.borrow()),
            _ => Err(EvalError::type_error("Object", self.clone())),
        }
    }
    pub fn use_object_mut<T, F: FnOnce(&mut ObjectValue) -> EvalResult<T>>(
        &self,
        f: F,
    ) -> EvalResult<T> {
        match self {
            Value::Ref(RefValue::Object(obj)) => f(&mut obj.borrow_mut()),
            _ => Err(EvalError::type_error("Object", self.clone())),
        }
    }
    pub fn get_object_prop(&self, key: &ObjectKey) -> EvalResult {
        self.use_object(|obj| {
            obj.get(key)
                .cloned()
                .ok_or_else(|| EvalError::property_not_found(key.clone()))
        })
    }
    pub fn get_object_prop_str(&self, prop: &str) -> EvalResult {
        self.get_object_prop(&ObjectKey::new_str_from_str(prop))
    }
    pub fn get_object_prop_sym(&self, prop: &str) -> EvalResult {
        self.get_object_prop(&ObjectKey::Sym(Rc::new(prop.into())))
    }

    pub fn to_object_key(&self) -> EvalResult<ObjectKey> {
        match self {
            Value::Str(s) => Ok(ObjectKey::Str(s.clone())),
            Value::Sym(s) => Ok(ObjectKey::Sym(s.clone())),
            _ => Err(EvalError::type_error("String|Symbol", self.clone())),
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Ref(RefValue::Array(values)) => {
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
            Value::Ref(RefValue::Object(obj)) => {
                obj.borrow().fmt(f)?;
            }
            Value::Ref(RefValue::Fun { current_module, .. }) => {
                f.write_str("#fun")?;
                f.write_str("@")?;
                current_module.path.fmt(f)?;
            }
            Value::Bool(v) => {
                v.fmt(f)?;
            }
            Value::Null => {
                f.write_str("null")?;
            }
            Value::Int(v) => {
                v.fmt(f)?;
            }
            Value::Intrinsic(name) => {
                f.write_str("#fun:")?;
                name.fmt(f)?;
            }
            Value::Str(s) => {
                f.write_char('"')?;
                f.write_str(s)?;
                f.write_char('"')?;
            }
            Value::Sym(s) => {
                f.write_char('#')?;
                f.write_str(s)?;
            }
        }
        Ok(())
    }
}

impl TryFrom<&Value> for i32 {
    type Error = EvalError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(v) => Ok(*v),
            _ => Err(EvalError::TypeError("Int".into(), value.clone())),
        }
    }
}
impl TryFrom<&Value> for bool {
    type Error = EvalError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(v) => Ok(*v),
            _ => Err(EvalError::TypeError("Bool".into(), value.clone())),
        }
    }
}
impl TryFrom<&Value> for usize {
    type Error = EvalError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        let value: i32 = value.try_into()?;
        value
            .try_into()
            .map_err(|_| EvalError::cast_error("usize", value))
    }
}
impl TryFrom<&Value> for String {
    type Error = EvalError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Str(s) => Ok((**s).clone()),
            _ => Err(EvalError::type_error("String", value.clone())),
        }
    }
}
impl TryFrom<Value> for ObjectKey {
    type Error = EvalError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Sym(s) => Ok(ObjectKey::Sym(s.clone())),
            Value::Str(s) => Ok(ObjectKey::Str(s.clone())),
            _ => Err(EvalError::type_error("String|Symbol", value)),
        }
    }
}

impl TryFrom<Value> for ObjectValueRef {
    type Error = EvalError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Ref(RefValue::Object(o)) => Ok(o),
            _ => Err(EvalError::type_error("Object", value)),
        }
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Int(value)
    }
}
impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::bool(value)
    }
}
impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(value: Vec<T>) -> Self {
        Value::array(value.into_iter().map(|v| v.into()).collect())
    }
}
impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Str(Rc::new(value.into()))
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
            return Err(EvalError::NameDefined(name.to_string()));
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

#[derive(PartialEq, Eq, Clone, Finalize)]
pub enum RefValue {
    Fun {
        params: Vec<Ident>,
        body: Box<Expr>,
        local_env: Option<LocalEnvRef>,
        current_module: Gc<Module>,
    },
    Object(Gc<GcCell<ObjectValue>>),
    Array(Gc<GcCell<Vec<Value>>>),
}
impl From<RefValue> for Value {
    fn from(value: RefValue) -> Self {
        Value::Ref(value)
    }
}
impl std::fmt::Debug for RefValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fun {
                params,
                body,
                local_env: _,
                current_module: _,
            } => f
                .debug_struct("Fun")
                .field("params", params)
                .field("body", body)
                .finish_non_exhaustive(),
            Self::Array(a) => f.debug_tuple("Array").field(a).finish(),
            Self::Object(o) => f.debug_tuple("Object").field(o).finish(),
        }
    }
}

unsafe impl Trace for RefValue {
    gc::custom_trace!(this, {
        match this {
            RefValue::Fun {
                params: _,
                body: _,
                local_env,
                current_module,
            } => {
                mark(local_env);
                mark(current_module);
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ObjectKey {
    Sym(Rc<String>),
    Str(Rc<String>),
}
impl ObjectKey {
    pub fn new_str_from_str<S: Into<String>>(name: S) -> Self {
        Self::Str(Rc::new(name.into()))
    }
}
impl std::fmt::Display for ObjectKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(s) => s.fmt(f),
            Self::Sym(s) => {
                f.write_str("#")?;
                s.fmt(f)
            }
        }
    }
}

pub type ArrayValueRef = Gc<GcCell<Vec<Value>>>;
pub type ObjectValueRef = Gc<GcCell<ObjectValue>>;

#[derive(PartialEq, Eq, Clone, Finalize)]
pub struct ObjectValue {
    names: Vec<ObjectKey>,
    values: HashMap<ObjectKey, Value>,
}
impl Default for ObjectValue {
    fn default() -> Self {
        Self::new()
    }
}
impl From<ObjectValue> for Value {
    fn from(value: ObjectValue) -> Self {
        RefValue::Object(Gc::new(GcCell::new(value))).into()
    }
}
impl std::fmt::Debug for ObjectValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = &mut f.debug_struct("ObjectValue");
        for (k, v) in self.iter() {
            f = f.field(&format!("{k}"), v);
        }
        f.finish()?;
        Ok(())
    }
}

impl ObjectValue {
    pub fn new() -> ObjectValue {
        ObjectValue {
            names: Vec::new(),
            values: HashMap::new(),
        }
    }
    pub fn insert(&mut self, name: ObjectKey, value: Value) {
        let old = self.values.insert(name.clone(), value);
        if old.is_none() {
            self.names.push(name);
        }
    }
    pub fn get(&self, name: &ObjectKey) -> Option<&Value> {
        self.values.get(name)
    }
    pub fn get_or_err(&self, name: &ObjectKey) -> EvalResult {
        self.get(name)
            .ok_or_else(|| EvalError::property_not_found(name.clone()))
            .cloned()
    }
    pub fn iter(&self) -> impl Iterator<Item = (&ObjectKey, &Value)> {
        ObjectIter {
            values: &self.values,
            names_it: self.names.iter(),
        }
    }
    pub fn contains_key(&self, key: &ObjectKey) -> bool {
        self.values.contains_key(key)
    }
}
pub struct ObjectIter<'a, I: Iterator<Item = &'a ObjectKey>> {
    names_it: I,
    values: &'a HashMap<ObjectKey, Value>,
}
impl<'a, I: Iterator<Item = &'a ObjectKey>> Iterator for ObjectIter<'a, I> {
    type Item = (&'a ObjectKey, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        let Some(name) = self.names_it.next() else {
            return None;
        };
        let value = self.values.get(name).unwrap();
        Some((name, value))
    }
}
unsafe impl Trace for ObjectValue {
    gc::custom_trace!(this, {
        for v in this.values.values() {
            mark(v);
        }
    });
}
impl std::fmt::Display for ObjectValue {
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
    use crate::{
        array_value,
        module::{ModuleEnv, ModulePath, NullModuleLoader},
        object_value,
    };

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
        let mut me = ModuleEnv::new(NullModuleLoader);
        assert_display!(
            Into::<Value>::into(RefValue::Fun {
                params: vec![],
                body: Box::new(Expr::Var("".into())),
                local_env: None,
                current_module: me.new_module(ModulePath::new("foo.bar")),
            }),
            "#fun@foo.bar"
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
        assert_display!(Value::Intrinsic("foo".into()), "#fun:foo");
    }
}
