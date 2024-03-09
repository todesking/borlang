use crate::ast::TopTerm;
use crate::value::AtomValue;
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
    NameNotFound(String),
    ArgumentLength(usize),
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
    vars: HashMap<String, Value>,
    intrinsics: HashMap<String, Box<IntrinsicFn>>,
}

fn binop_int<F: Fn(i32, i32) -> EvalResult>(f: F) -> impl Fn(&[Value]) -> EvalResult {
    move |args| match args {
        [Value::Atom(AtomValue::Int(lhs)), Value::Atom(AtomValue::Int(rhs))] => f(*lhs, *rhs),
        [Value::Atom(AtomValue::Int(_)), rhs] => {
            Err(EvalError::TypeError("Int".to_owned(), rhs.clone()))
        }
        [lhs, _] => Err(EvalError::TypeError("Int".to_owned(), lhs.clone())),
        _ => Err(EvalError::ArgumentLength(args.len())),
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

    pub fn register_instrinsic<S: Into<String>, F: 'static + Fn(&[Value]) -> EvalResult>(
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
                let value = self.eval_expr(expr)?;
                self.set_var(&name.0, value);
            }
        }
        Ok(())
    }
    pub fn eval_expr(&mut self, expr: &Expr) -> EvalResult {
        match expr {
            Expr::AtomValue(v) => Ok(v.clone().into()),
            Expr::Var(name) => self.get_var(&name.0),
            Expr::Block { terms, expr } => {
                for e in terms {
                    self.eval_expr(e)?;
                }
                if let Some(e) = expr {
                    self.eval_expr(e)
                } else {
                    Ok(Value::null())
                }
            }
            Expr::App { f, args } => {
                let f = self.eval_expr(f)?;
                let args = args
                    .iter()
                    .map(|a| self.eval_expr(a))
                    .collect::<Result<Vec<_>, _>>()?;
                self.eval_app(&f, &args)
            }
            Expr::Let { name, expr } => {
                let value = self.eval_expr(expr)?;
                self.set_var(&name.0, value);
                Ok(Value::null())
            }
            Expr::Fun { params, expr } => Ok(Value::fun(params.clone(), expr.clone())),
        }
    }

    pub fn eval_to_int(&mut self, expr: &Expr) -> Result<i32, EvalError> {
        self.eval_expr(expr)?.try_into()
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
                RefValue::Fun { params: _, body } => self.eval_expr(body),
            },
            _ => Err(EvalError::TypeError("Intrinsic".to_owned(), f.clone())),
        }
    }

    pub fn set_var<S: Into<String>>(&mut self, name: S, value: Value) {
        self.vars.insert(name.into(), value);
    }

    pub fn get_var<S: AsRef<str> + Into<String>>(&self, name: S) -> EvalResult {
        self.vars
            .get(name.as_ref())
            .cloned()
            .ok_or_else(|| EvalError::NameNotFound(name.into()))
    }
}
