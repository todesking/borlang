use crate::ast::ArrayItem;
use crate::ast::Ident;
use crate::ast::ObjItem;
use crate::ast::TopTerm;
use crate::value::AtomValue;
use crate::value::LocalEnv;
use crate::value::LocalEnvRef;
use crate::value::ObjectValue;
use crate::value::RefValue;
use crate::EvalError;
use crate::Expr;
use crate::Program;
use crate::Value;

use std::collections::HashMap;

pub type EvalResult<T = Value> = Result<T, EvalError>;
pub type IntrinsicFn = dyn Fn(&[Value]) -> EvalResult;

pub struct Env {
    vars: HashMap<Ident, Value>,
    intrinsics: HashMap<Ident, Box<IntrinsicFn>>,
    allow_rebind_global: bool,
    array_proto: ObjectValue,
}
impl Default for Env {
    fn default() -> Self {
        Env::new()
    }
}

fn binop_any<F: Fn(&Value, &Value) -> EvalResult>(f: F) -> impl Fn(&[Value]) -> EvalResult {
    move |args: &[Value]| match args {
        [lhs, rhs] => f(lhs, rhs),
        _ => Err(EvalError::ArgumentLength {
            expected: 2,
            actual: args.len(),
        }),
    }
}

fn binop<
    L: for<'a> TryFrom<&'a Value, Error = EvalError>,
    R: for<'a> TryFrom<&'a Value, Error = EvalError>,
    F: Fn(L, R) -> EvalResult,
>(
    f: F,
) -> impl Fn(&[Value]) -> EvalResult {
    binop_any(move |lhs, rhs| f(lhs.try_into()?, rhs.try_into()?))
}

impl Env {
    pub fn new() -> Env {
        let mut env = Env {
            vars: Default::default(),
            intrinsics: Default::default(),
            allow_rebind_global: false,
            array_proto: ObjectValue::new(),
        };
        env.register_instrinsic("#array_len", |args| match args {
            [this] => this.use_array(|arr| Ok((arr.len() as i32).into())),
            _ => Err(EvalError::ArgumentLength {
                expected: 1,
                actual: args.len(),
            }),
        });
        env.array_proto
            .insert("len".into(), Value::intrinsic("#array_len"));
        env.register_instrinsic("#array_push", |args| match args {
            [this, value] => this.use_array_mut(|arr| {
                arr.push(value.clone());
                Ok(Value::null())
            }),
            _ => Err(EvalError::ArgumentLength {
                expected: 2,
                actual: args.len(),
            }),
        });
        env.array_proto
            .insert("push".into(), Value::intrinsic("#array_push"));
        env.register_instrinsic("+", binop(|lhs: i32, rhs: i32| Ok(Value::int(lhs + rhs))));
        env.register_instrinsic("-", binop(|lhs: i32, rhs: i32| Ok(Value::int(lhs - rhs))));
        env.register_instrinsic("*", binop(|lhs: i32, rhs: i32| Ok(Value::int(lhs * rhs))));
        env.register_instrinsic("%", binop(|lhs: i32, rhs: i32| Ok(Value::int(lhs % rhs))));
        env.register_instrinsic("==", binop_any(|lhs, rhs| Ok((lhs == rhs).into())));
        env.register_instrinsic("!=", binop_any(|lhs, rhs| Ok((lhs != rhs).into())));
        env.register_instrinsic("#unary-", |args| {
            if args.len() != 1 {
                return Err(EvalError::ArgumentLength {
                    expected: 1,
                    actual: args.len(),
                });
            }
            let v = i32::try_from(&args[0])?;
            Ok((-v).into())
        });
        env.bind_global("true", true).unwrap();
        env.bind_global("false", false).unwrap();
        env.bind_global("null", Value::null()).unwrap();
        env
    }
    pub fn register_instrinsic<S: Into<Ident>, F: 'static + Fn(&[Value]) -> EvalResult>(
        &mut self,
        name: S,
        f: F,
    ) {
        let name = name.into();
        self.intrinsics.insert(name.clone(), Box::new(f));
        self.vars
            .insert(name.clone(), Value::intrinsic(name.clone()));
    }
    pub fn eval_program(&mut self, program: &Program) -> Result<(), EvalError> {
        for t in &program.top_terms {
            self.eval_top_term(t)?;
        }
        Ok(())
    }
    pub fn eval_top_term(&mut self, top_term: &TopTerm) -> Result<(), EvalError> {
        match top_term {
            TopTerm::Let { name, expr } => {
                let value = self.eval_expr(expr, &None)?;
                self.bind_global(name.clone(), value)?;
            }
        }
        Ok(())
    }
    pub fn eval_expr(&mut self, expr: &Expr, local_env: &Option<LocalEnvRef>) -> EvalResult {
        match expr {
            Expr::Int(v) => Ok(AtomValue::Int(*v).into()),
            Expr::Str { content } => Ok(AtomValue::Str(content.clone()).into()),
            Expr::Object(items) => {
                let mut buf = ObjectValue::new();
                for item in items {
                    match item {
                        ObjItem::Kv(name, expr) => {
                            let value = self.eval_expr(expr, local_env)?;
                            buf.insert(name.clone(), value);
                        }
                        ObjItem::Spread(expr) => {
                            let value = self.eval_expr(expr, local_env)?;
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
                    let value = self.eval_expr(expr, local_env)?;
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
            Expr::Var(name) => self.get_var(local_env, name),
            Expr::Paren { expr } => self.eval_expr(expr, local_env),
            Expr::Block { terms, expr } => {
                let local_env = Some(LocalEnv::extend(local_env.clone()));
                for e in terms {
                    self.eval_expr(e, &local_env)?;
                }
                if let Some(e) = expr {
                    self.eval_expr(e, &local_env)
                } else {
                    Ok(Value::null())
                }
            }
            Expr::Binop { lhs, op, rhs } => {
                let lhs = self.eval_expr(lhs, local_env)?;
                let rhs = self.eval_expr(rhs, local_env)?;
                let f = self.get_var(local_env, op)?;
                self.eval_app(&f, &[lhs, rhs])
            }
            Expr::Negate { expr } => {
                let f = self.get_var(local_env, &Ident::new("#unary-"))?;
                let v = self.eval_expr(expr, local_env)?;
                self.eval_app(&f, &[v])
            }
            Expr::App { expr: f, args } => match &**f {
                Expr::Prop { expr, name } => {
                    let this = self.eval_expr(expr, local_env)?;
                    let f = self.get_prop(&this, name)?;
                    let mut args = self.eval_args(args, local_env)?;
                    args.insert(0, this);
                    self.eval_app(&f, &args)
                }
                _ => {
                    let f = self.eval_expr(f, local_env)?;
                    let args = self.eval_args(args, local_env)?;
                    self.eval_app(&f, &args)
                }
            },
            Expr::Let { name, expr } => {
                let value = self.eval_expr(expr, local_env)?;
                self.bind_var(local_env, name.clone(), value)?;
                Ok(Value::null())
            }
            Expr::Reassign { lhs, rhs } => {
                match &**lhs {
                    Expr::Var(name) => {
                        let value = self.eval_expr(rhs, local_env)?;
                        self.reassign_var(local_env, name, value)?;
                    }
                    Expr::Index { expr, index } => {
                        let arr = self.eval_expr(expr, local_env)?;
                        let index = (&self.eval_expr(index, local_env)?).try_into()?;
                        let value = self.eval_expr(rhs, local_env)?;
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
                        let obj = self.eval_expr(expr, local_env)?;
                        let value = self.eval_expr(rhs, local_env)?;
                        obj.use_object_mut(|obj| {
                            obj.insert(name.clone(), value);
                            Ok(Value::null())
                        })?;
                    }
                    _ => return Err(EvalError::AssignNotSupported),
                }
                Ok(Value::null())
            }
            Expr::If { cond, th, el } => {
                let cond = self.eval_expr(cond, local_env)?;
                let cond = (&cond).try_into()?;
                let value = if cond {
                    self.eval_expr(th, local_env)?
                } else {
                    el.as_ref()
                        .map(|el| self.eval_expr(el, local_env))
                        .unwrap_or_else(|| Ok(Value::null()))?
                };
                Ok(value)
            }
            Expr::Fun { params, expr } => {
                Ok(Value::fun(params.clone(), expr.clone(), local_env.clone()))
            }
            Expr::Prop { expr, name } => {
                let value = self.eval_expr(expr, local_env)?;
                self.get_prop(&value, name)
            }
            Expr::Index { expr, index } => {
                let value = self.eval_expr(expr, local_env)?;
                let index = self.eval_expr(index, local_env)?;
                self.get_index(&value, &index)
            }
        }
    }

    fn get_prop(&self, value: &Value, name: &Ident) -> EvalResult {
        self.read_object(value, |obj| {
            obj.get(name)
                .cloned()
                .ok_or_else(|| EvalError::PropertyNotFound(name.clone()))
        })
    }

    fn get_index(&self, value: &Value, index: &Value) -> EvalResult {
        let index: usize = index.try_into()?;
        value.use_array(|values| {
            if index < values.len() {
                Ok(values[index].clone())
            } else {
                Err(EvalError::IndexOutOfBound {
                    len: values.len(),
                    index,
                })
            }
        })
    }

    fn eval_args(
        &mut self,
        args: &[Expr],
        local_env: &Option<LocalEnvRef>,
    ) -> Result<Vec<Value>, EvalError> {
        args.iter()
            .map(|a| self.eval_expr(a, local_env))
            .collect::<Result<Vec<_>, _>>()
    }

    fn read_object<F: FnOnce(&ObjectValue) -> EvalResult>(
        &self,
        value: &Value,
        f: F,
    ) -> EvalResult {
        match value {
            Value::Atom(_) => todo!(),
            Value::Ref(ref_value) => match &**ref_value {
                RefValue::Object(obj) => {
                    let obj = obj.borrow();
                    f(&obj)
                }
                RefValue::Array(_) => f(&self.array_proto),
                _ => todo!(),
            },
        }
    }

    fn eval_app(&mut self, f: &Value, args: &[Value]) -> EvalResult {
        match f {
            Value::Atom(AtomValue::Intrinsic(name)) => {
                let f = self
                    .intrinsics
                    .get(name)
                    .expect("Intrinsic should registered");
                f(args)
            }
            Value::Ref(v) => match &**v {
                RefValue::Fun {
                    params,
                    body,
                    local_env,
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
                    self.eval_expr(body, &Some(local_env))
                }
                _ => Err(EvalError::TypeError("Fun".into(), f.clone())),
            },
            _ => Err(EvalError::TypeError("Intrinsic".to_owned(), f.clone())),
        }
    }

    pub fn allow_rebind_global(&mut self, v: bool) {
        self.allow_rebind_global = v;
    }

    pub fn bind_global<I: Into<Ident>, V: Into<Value>>(
        &mut self,
        name: I,
        value: V,
    ) -> Result<(), EvalError> {
        let name = name.into();
        if !self.allow_rebind_global && self.vars.contains_key(&name) {
            return Err(EvalError::NameDefined(name));
        }
        self.vars.insert(name, value.into());
        Ok(())
    }

    pub fn bind_var(
        &mut self,
        local_env: &Option<LocalEnvRef>,
        name: Ident,
        value: Value,
    ) -> Result<(), EvalError> {
        if let Some(local_env) = local_env {
            LocalEnv::bind(local_env, name.clone(), value)
        } else {
            self.bind_global(name, value)
        }
    }

    fn reassign_var(
        &mut self,
        local_env: &Option<LocalEnvRef>,
        name: &Ident,
        value: Value,
    ) -> Result<(), EvalError> {
        match LocalEnv::reassign_if_exists(local_env, name, value) {
            Ok(_) => Ok(()),
            Err(value) => self.reassign_global(name, value),
        }
    }

    pub fn reassign_global(&mut self, name: &Ident, value: Value) -> Result<(), EvalError> {
        let Some(v) = self.vars.get_mut(name) else {
            return Err(EvalError::NameNotFound(name.to_owned()));
        };
        *v = value;
        Ok(())
    }

    pub fn get_var(&self, local_env: &Option<LocalEnvRef>, name: &Ident) -> EvalResult {
        LocalEnv::get_var(local_env, name)
            .or_else(|| self.vars.get(name).cloned())
            .ok_or_else(|| EvalError::NameNotFound(name.clone()))
    }
}
