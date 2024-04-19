use crate::ast::ArrayItem;
use crate::ast::Ident;
use crate::ast::ObjItem;
use crate::intrinsic::register_intrinsics;
use crate::module::Module;
use crate::module::ModuleEnv;
use crate::module::ModuleLoader;
use crate::module::ModulePath;
use crate::object_value;
use crate::value::AtomValue;
use crate::value::LocalEnv;
use crate::value::LocalEnvRef;
use crate::value::ObjectKey;
use crate::value::ObjectValue;
use crate::value::RefValue;
use crate::EvalError;
use crate::Expr;

use crate::Value;
use gc::Gc;

use std::collections::HashMap;

pub type EvalResult<T = Value> = Result<T, EvalError>;
pub type IntrinsicFn = dyn Fn(&[Value]) -> EvalResult;

pub struct RuntimeContext<Loader: ModuleLoader> {
    module_env: ModuleEnv<Loader>,
    intrinsics: HashMap<String, fn(&[Value]) -> EvalResult>,
    allow_rebind_global: bool,
    array_proto: ObjectValue,
    prelude: Gc<Module>,
}

impl<L: ModuleLoader> RuntimeContext<L> {
    pub fn new(loader: L) -> Self {
        let mut intrinsics = HashMap::<String, fn(&[Value]) -> EvalResult>::new();
        macro_rules! intrinsic {
            ($name:ident, $args:ident, $body:expr) => {{
                fn $name($args: &[Value]) -> EvalResult {
                    $body
                }
                let name = stringify!($name).to_owned();
                intrinsics.insert(name.clone(), $name);
                Value::intrinsic(name)
            }};
        }

        let array_proto = object_value! {
            len: intrinsic!(array_len, args, {match args {
                [this] => this.use_array(|arr| Ok((arr.len() as i32).into())),
                _ => Err(EvalError::argument_length(1, args.len())),
            }}),
            push: intrinsic!(array_push, args, { match args {
                [this, value] => this.use_array_mut(|arr| {
                    arr.push(value.clone());
                    Ok(Value::null())
                }),
                _ => Err(EvalError::argument_length(2, args.len())),
            }}),
            iterator: intrinsic!(array_iterator, args, {
                EvalError::check_argument_len(1, args.len())?;
                let this = &args[0];
                let mut iter = ObjectValue::new();
                iter.insert(
                    ObjectKey::new_str_from_str("next"),
                    Value::intrinsic("array_iterator_next"),
                );
                iter.insert(ObjectKey::new_str_from_str("data"), this.clone());
                iter.insert(ObjectKey::new_str_from_str("cur"), Value::int(0));
                Ok(iter.into())
            })
        };

        let mut module_env = ModuleEnv::new(loader);
        let prelude = module_env.new_module(ModulePath::new("std.prelude"));
        prelude
            .bind("true", Value::bool(true), true, false)
            .unwrap();
        prelude
            .bind("false", Value::bool(false), true, false)
            .unwrap();
        prelude.bind("null", Value::null(), true, false).unwrap();

        let mut rt = Self {
            module_env,
            intrinsics,
            allow_rebind_global: false,
            array_proto,
            prelude,
        };
        register_intrinsics(&mut rt);
        rt
    }

    pub fn register_intrinsic<S: Into<String>>(
        &mut self,
        name: S,
        f: fn(&[Value]) -> EvalResult,
    ) -> Value {
        let name = name.into();
        self.intrinsics.insert(name.clone(), f);
        Value::intrinsic(name)
    }

    pub fn allow_rebind_global(&mut self, v: bool) {
        self.allow_rebind_global = v;
    }

    pub fn new_module(&mut self, path: ModulePath) -> Gc<Module> {
        let m = self.module_env.new_module(path);
        self.prelude
            .pub_object()
            .use_object(|o| {
                for (name, value) in o.iter() {
                    if let ObjectKey::Str(name) = name {
                        m.bind(&**name, value.clone(), false, false)?;
                    }
                }
                Ok(())
            })
            .unwrap();
        m
    }

    pub fn eval_expr_in_module(&mut self, expr: &Expr, module: &Gc<Module>) -> EvalResult {
        self.eval_expr(expr, &None, module)
    }

    fn eval_expr(
        &mut self,
        expr: &Expr,
        local_env: &Option<LocalEnvRef>,
        current_module: &Gc<Module>,
    ) -> EvalResult {
        match expr {
            Expr::Int(v) => Ok(AtomValue::Int(*v).into()),
            Expr::Str { content } => Ok(AtomValue::Str(content.clone()).into()),
            Expr::Object(items) => {
                let mut buf = ObjectValue::new();
                for item in items {
                    match item {
                        ObjItem::Kv {
                            name,
                            expr: Some(expr),
                        } => {
                            let value = self.eval_expr(expr, local_env, current_module)?;
                            buf.insert(name.to_object_key(), value);
                        }
                        ObjItem::Kv { name, expr: None } => {
                            let value = self.get_var(local_env, current_module, name)?;
                            buf.insert(name.to_object_key(), value);
                        }
                        ObjItem::Spread(expr) => {
                            let value = self.eval_expr(expr, local_env, current_module)?;
                            value.use_object(|obj| {
                                for (name, value) in obj.iter() {
                                    buf.insert(name.clone(), value.clone());
                                }
                                Ok(())
                            })?;
                        }
                    }
                }
                Ok(Value::object(buf))
            }
            Expr::Array(items) => {
                let mut buf = Vec::new();
                for ArrayItem { spread, expr } in items {
                    let value = self.eval_expr(expr, local_env, current_module)?;
                    if spread.is_some() {
                        value.use_array(|arr| {
                            buf.extend(arr.iter().cloned());
                            Ok(())
                        })?;
                    } else {
                        buf.push(value);
                    }
                }
                Ok(Value::array(buf))
            }
            Expr::Var(name) => self.get_var(local_env, current_module, name),
            Expr::Paren { expr } => self.eval_expr(expr, local_env, current_module),
            Expr::Block { terms, expr } => {
                let local_env = Some(LocalEnv::extend(local_env.clone()));
                for e in terms {
                    self.eval_expr(e, &local_env, current_module)?;
                }
                if let Some(e) = expr {
                    self.eval_expr(e, &local_env, current_module)
                } else {
                    Ok(Value::null())
                }
            }
            Expr::Binop { lhs, op, rhs } => {
                let lhs = self.eval_expr(lhs, local_env, current_module)?;
                let rhs = self.eval_expr(rhs, local_env, current_module)?;
                let intrinsic_name = match &**op.0 {
                    "+" => "op_plus",
                    "-" => "op_minus",
                    "*" => "op_mul",
                    "%" => "op_mod",
                    "==" => "op_eq",
                    "!=" => "op_ne",
                    unk => panic!("Unknown binary operator: {unk}"),
                };
                let f = Value::intrinsic(intrinsic_name);
                self.eval_app(&f, &[lhs, rhs])
            }
            Expr::Negate { expr } => {
                let f = Value::intrinsic("op_negate");
                let v = self.eval_expr(expr, local_env, current_module)?;
                self.eval_app(&f, &[v])
            }
            Expr::App { expr: f, args } => match &**f {
                Expr::Prop { expr, name } => {
                    let this = self.eval_expr(expr, local_env, current_module)?;
                    let f = self.get_prop(&this, &name.to_object_key())?;
                    let mut args = self.eval_args(args, local_env, current_module)?;
                    args.insert(0, this);
                    self.eval_app(&f, &args)
                }
                _ => {
                    let f = self.eval_expr(f, local_env, current_module)?;
                    let args = self.eval_args(args, local_env, current_module)?;
                    self.eval_app(&f, &args)
                }
            },
            Expr::Let { name, expr } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                self.bind_var(local_env, current_module, name.clone(), value)?;
                Ok(Value::null())
            }
            Expr::Reassign { lhs, rhs } => {
                match &**lhs {
                    Expr::Var(name) => {
                        let value = self.eval_expr(rhs, local_env, current_module)?;
                        self.reassign_var(local_env, current_module, name, value)?;
                    }
                    Expr::Index { expr, index } => {
                        let arr = self.eval_expr(expr, local_env, current_module)?;
                        let index =
                            (&self.eval_expr(index, local_env, current_module)?).try_into()?;
                        let value = self.eval_expr(rhs, local_env, current_module)?;
                        arr.use_array_mut(|arr| {
                            if arr.len() <= index {
                                return Err(EvalError::IndexOutOfBound {
                                    len: arr.len(),
                                    index,
                                });
                            }
                            arr[index] = value;
                            Ok(Value::null())
                        })?;
                    }
                    Expr::Prop { expr, name } => {
                        let obj = self.eval_expr(expr, local_env, current_module)?;
                        let value = self.eval_expr(rhs, local_env, current_module)?;
                        obj.use_object_mut(|obj| {
                            obj.insert(name.to_object_key(), value);
                            Ok(Value::null())
                        })?;
                    }
                    _ => return Err(EvalError::AssignNotSupported),
                }
                Ok(Value::null())
            }
            Expr::If { cond, th, el } => {
                let cond = self.eval_expr(cond, local_env, current_module)?;
                let cond = (&cond).try_into()?;
                let value = if cond {
                    self.eval_expr(th, local_env, current_module)?
                } else {
                    el.as_ref()
                        .map(|el| self.eval_expr(el, local_env, current_module))
                        .unwrap_or_else(|| Ok(Value::null()))?
                };
                Ok(value)
            }
            Expr::For { name, target, body } => {
                let target = self.eval_expr(target, local_env, current_module)?;
                let iter_new = self.get_prop(&target, &ObjectKey::new_str_from_str("iterator"))?;
                let iter = self.eval_app(&iter_new, &[target])?;
                let args = [iter];
                loop {
                    let next = self.get_prop(&args[0], &ObjectKey::new_str_from_str("next"))?;
                    let next_value = self.eval_app(&next, &args)?;
                    let next_value = next_value.use_array(|a| match a {
                        [] => Ok(None),
                        [v] => Ok(Some(v.clone())),
                        _ => Err(EvalError::type_error("[] | [T]", next_value.clone())),
                    })?;
                    let Some(next_value) = next_value else {
                        break;
                    };
                    let env = LocalEnv::extend(local_env.clone());
                    LocalEnv::bind(&env, name.clone(), next_value.clone())?;
                    self.eval_expr(body, &Some(env), current_module)?;
                }
                Ok(Value::null())
            }
            Expr::Fun { params, expr } => Ok(Value::fun(
                params.clone(),
                expr.clone(),
                local_env.clone(),
                current_module.clone(),
            )),
            Expr::Prop { expr, name } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                self.get_prop(&value, &name.to_object_key())
            }
            Expr::PropOpt { expr, name } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                if value.is_null() {
                    Ok(Value::null())
                } else {
                    self.get_prop(&value, &name.to_object_key())
                }
            }
            Expr::Index { expr, index } => {
                let value = self.eval_expr(expr, local_env, current_module)?;
                let index = self.eval_expr(index, local_env, current_module)?;
                self.get_index(&value, &index)
            }
        }
    }

    fn eval_app(&mut self, f: &Value, args: &[Value]) -> EvalResult {
        match f {
            Value::Atom(AtomValue::Intrinsic(name)) => {
                let f = self
                    .intrinsics
                    .get(&*name.0)
                    .expect("Intrinsic should registered");
                f(args)
            }
            Value::Ref(v) => match &**v {
                RefValue::Fun {
                    params,
                    body,
                    local_env,
                    current_module,
                } => {
                    if params.len() != args.len() {
                        return Err(EvalError::ArgumentLength {
                            expected: params.len(),
                            actual: args.len(),
                        });
                    }
                    let local_env = LocalEnv::extend(local_env.clone());
                    for (param, arg) in params.iter().zip(args.iter()) {
                        LocalEnv::bind(&local_env, param.clone(), arg.clone())?;
                    }
                    self.eval_expr(body, &Some(local_env), current_module)
                }
                _ => Err(EvalError::TypeError("Fun".into(), f.clone())),
            },
            _ => Err(EvalError::TypeError("Intrinsic".to_owned(), f.clone())),
        }
    }

    pub fn get_var(
        &self,
        local_env: &Option<LocalEnvRef>,
        current_module: &Module,
        name: &Ident,
    ) -> EvalResult {
        LocalEnv::get_var(local_env, name)
            .or_else(|| current_module.lookup(&name.0))
            .ok_or_else(|| EvalError::NameNotFound(name.to_string()))
    }
    fn get_index(&self, value: &Value, index: &Value) -> EvalResult {
        let index: usize = index.try_into()?;
        value.use_array(|values| {
            EvalError::check_array_index(values.len(), index)?;
            Ok(values[index].clone())
        })
    }

    fn eval_args(
        &mut self,
        args: &[Expr],
        local_env: &Option<LocalEnvRef>,
        current_module: &Gc<Module>,
    ) -> Result<Vec<Value>, EvalError> {
        args.iter()
            .map(|a| self.eval_expr(a, local_env, current_module))
            .collect::<Result<Vec<_>, _>>()
    }

    fn read_object<F: FnOnce(&ObjectValue) -> EvalResult>(
        &self,
        value: &Value,
        f: F,
    ) -> EvalResult {
        match value {
            Value::Atom(_) => f(&ObjectValue::new()),
            Value::Ref(ref_value) => match &**ref_value {
                RefValue::Object(obj) => {
                    let obj = obj.borrow();
                    f(&obj)
                }
                RefValue::Array(_) => f(&self.array_proto),
                _ => f(&ObjectValue::new()),
            },
        }
    }
    fn get_prop(&self, value: &Value, key: &ObjectKey) -> EvalResult {
        self.read_object(value, |obj| {
            obj.get(key)
                .cloned()
                .ok_or_else(|| EvalError::PropertyNotFound(key.clone()))
        })
    }
    pub fn bind_var(
        &mut self,
        local_env: &Option<LocalEnvRef>,
        current_module: &Module,
        name: Ident,
        value: Value,
    ) -> Result<(), EvalError> {
        if let Some(local_env) = local_env {
            LocalEnv::bind(local_env, name.clone(), value)
        } else {
            current_module.bind((*name.0).clone(), value, false, self.allow_rebind_global)
        }
    }

    fn reassign_var(
        &mut self,
        local_env: &Option<LocalEnvRef>,
        current_module: &Module,
        name: &Ident,
        value: Value,
    ) -> Result<(), EvalError> {
        match LocalEnv::reassign_if_exists(local_env, name, value) {
            Ok(_) => Ok(()),
            Err(value) => current_module.reassign((*name.0).clone(), value),
        }
    }
}
