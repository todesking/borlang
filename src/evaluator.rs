use crate::ast::TopTerm;
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
    IllegalArgumentLength(usize),
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
        [Value::Int(lhs), Value::Int(rhs)] => f(*lhs, *rhs),
        [Value::Int(_), rhs] => Err(EvalError::TypeError("Int".to_owned(), rhs.clone())),
        [lhs, Value::Int(_)] => Err(EvalError::TypeError("Int".to_owned(), lhs.clone())),
        _ => Err(EvalError::IllegalArgumentLength(args.len())),
    }
}

impl Env {
    pub fn new() -> Env {
        Default::default()
    }
    pub fn prelude() -> Env {
        let mut env = Env::new();
        env.register_instrinsic("+", binop_int(|lhs, rhs| Ok(Value::Int(lhs + rhs))));
        env.register_instrinsic("-", binop_int(|lhs, rhs| Ok(Value::Int(lhs - rhs))));
        env.register_instrinsic("*", binop_int(|lhs, rhs| Ok(Value::Int(lhs * rhs))));
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
            .insert(name.clone(), Value::Intrinsic(name.clone()));
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
            Expr::Value(v) => Ok(v.clone()),
            Expr::Var(name) => self.get_var(&name.0),
            Expr::Block { terms, expr } => {
                for e in terms {
                    self.eval_expr(e)?;
                }
                if let Some(e) = expr {
                    self.eval_expr(e)
                } else {
                    Ok(Value::Null)
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
                Ok(Value::Null)
            }
        }
    }

    pub fn eval_to_int(&mut self, expr: &Expr) -> Result<i32, EvalError> {
        match self.eval_expr(expr)? {
            Value::Int(n) => Ok(n),
            other => Err(EvalError::TypeError("Not int".into(), other)),
        }
    }

    fn eval_app(&mut self, f: &Value, args: &[Value]) -> EvalResult {
        match f {
            Value::Intrinsic(name) => {
                let f = self
                    .intrinsics
                    .get(name)
                    .expect("Intrinsic should registered");
                f(args)
            }
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
