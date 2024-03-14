use crate::ast::Ident;
use crate::ast::TopTerm;
use crate::value::AtomValue;
use crate::value::LocalEnv;
use crate::value::LocalEnvRef;
use crate::value::RefValue;
use crate::Expr;
use crate::Program;
use crate::Value;

use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq)]
pub enum EvalError {
    TypeError(String, Value),
    NameNotFound(Ident),
    NameDefined(Ident),
    ArgumentLength { expected: usize, actual: usize },
}
impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
impl Error for EvalError {}

pub type EvalResult = Result<Value, EvalError>;
pub type IntrinsicFn = dyn Fn(&[Value]) -> EvalResult;

#[derive(Default)]
pub struct Env {
    vars: HashMap<Ident, Value>,
    intrinsics: HashMap<Ident, Box<IntrinsicFn>>,
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
        Default::default()
    }
    pub fn prelude() -> Env {
        let mut env = Env::new();
        env.register_instrinsic("+", binop(|lhs: i32, rhs: i32| Ok(Value::int(lhs + rhs))));
        env.register_instrinsic("-", binop(|lhs: i32, rhs: i32| Ok(Value::int(lhs - rhs))));
        env.register_instrinsic("*", binop(|lhs: i32, rhs: i32| Ok(Value::int(lhs * rhs))));
        env.register_instrinsic("==", binop_any(|lhs, rhs| Ok((lhs == rhs).into())));
        env.register_instrinsic("!=", binop_any(|lhs, rhs| Ok((lhs != rhs).into())));
        env.bind_global("true", true).unwrap();
        env.bind_global("false", false).unwrap();
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
            Expr::AtomValue(v) => Ok(v.clone().into()),
            Expr::Object { exprs } => {
                let mut values = HashMap::new();
                for (name, expr) in exprs.iter() {
                    let value = self.eval_expr(expr, local_env)?;
                    values.insert(name.clone(), value);
                }
                Ok(Value::object(values))
            }
            Expr::Var(name) => self.get_var(local_env, name),
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
            Expr::App { f, args } => {
                let f = self.eval_expr(f, local_env)?;
                let args = args
                    .iter()
                    .map(|a| self.eval_expr(a, local_env))
                    .collect::<Result<Vec<_>, _>>()?;
                self.eval_app(&f, &args)
            }
            Expr::Let { name, expr } => {
                let value = self.eval_expr(expr, local_env)?;
                self.bind_var(local_env, name.clone(), value)?;
                Ok(Value::null())
            }
            Expr::Reassign { name, expr } => {
                let value = self.eval_expr(expr, local_env)?;
                self.reassign_var(local_env, name, value)?;
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
                RefValue::Object { .. } => Err(EvalError::TypeError("Fun".into(), f.clone())),
            },
            _ => Err(EvalError::TypeError("Intrinsic".to_owned(), f.clone())),
        }
    }

    pub fn bind_global<I: Into<Ident>, V: Into<Value>>(
        &mut self,
        name: I,
        value: V,
    ) -> Result<(), EvalError> {
        let name = name.into();
        if self.vars.contains_key(&name) {
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
            LocalEnv::bind(local_env, name.clone(), value)?;
            Ok(())
        } else {
            if self.vars.contains_key(&name) {
                return Err(EvalError::NameDefined(name));
            }
            self.vars.insert(name, value);
            Ok(())
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
