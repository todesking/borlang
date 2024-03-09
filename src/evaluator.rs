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

fn binop_int<F: Fn(i32, i32) -> EvalResult>(f: F) -> impl Fn(&[Value]) -> EvalResult {
    move |args| match args {
        [Value::Atom(AtomValue::Int(lhs)), Value::Atom(AtomValue::Int(rhs))] => f(*lhs, *rhs),
        [Value::Atom(AtomValue::Int(_)), rhs] => {
            Err(EvalError::TypeError("Int".to_owned(), rhs.clone()))
        }
        [lhs, _] => Err(EvalError::TypeError("Int".to_owned(), lhs.clone())),
        _ => Err(EvalError::ArgumentLength {
            expected: 2,
            actual: args.len(),
        }),
    }
}

impl Env {
    pub fn new() -> Env {
        Default::default()
    }
    pub fn prelude() -> Env {
        let mut env = Env::new();
        env.register_instrinsic("+", binop_int(|lhs, rhs| Ok(Value::int(lhs + rhs))));
        env.register_instrinsic("-", binop_int(|lhs, rhs| Ok(Value::int(lhs - rhs))));
        env.register_instrinsic("*", binop_int(|lhs, rhs| Ok(Value::int(lhs * rhs))));
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
                self.set_var(&name.0, value);
            }
        }
        Ok(())
    }
    pub fn eval_expr(&mut self, expr: &Expr, local_env: &Option<LocalEnvRef>) -> EvalResult {
        match expr {
            Expr::AtomValue(v) => Ok(v.clone().into()),
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
                self.bind_var(local_env, &name.0, value);
                Ok(Value::null())
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
            Value::Ref(f) => match &**f {
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
                        LocalEnv::bind(&local_env, param.clone(), arg.clone());
                    }
                    self.eval_expr(body, &Some(local_env))
                }
            },
            _ => Err(EvalError::TypeError("Intrinsic".to_owned(), f.clone())),
        }
    }

    pub fn set_var<S: Into<Ident>>(&mut self, name: S, value: Value) {
        self.vars.insert(name.into(), value);
    }

    pub fn bind_var<S: Into<Ident>>(
        &mut self,
        local_env: &Option<LocalEnvRef>,
        name: S,
        value: Value,
    ) {
        if let Some(local_env) = local_env {
            LocalEnv::bind(local_env, name, value);
        } else {
            self.vars.insert(name.into(), value);
        }
    }

    pub fn get_var(&self, local_env: &Option<LocalEnvRef>, name: &Ident) -> EvalResult {
        LocalEnv::get_var(local_env, name)
            .or_else(|| self.vars.get(name).cloned())
            .ok_or_else(|| EvalError::NameNotFound(name.clone()))
    }
}
