use crate::syntax::grammar::OpAddSub;
use crate::syntax::grammar::OpMul;
use crate::Expr;
use crate::Program;
use crate::Value;
use std::error::Error;
use std::fmt::Debug;
use std::fmt::Display;

#[derive(Debug)]
pub enum EvalError {
    TypeError(String, Value),
}
impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
impl Error for EvalError {}

pub struct Env {}

impl Env {
    pub fn prelude() -> Env {
        Env {}
    }
    pub fn eval(&mut self, program: &Program) -> Result<Value, EvalError> {
        match program {
            Program::Expr(expr) => self.eval_expr(expr),
        }
    }
    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, EvalError> {
        match expr {
            Expr::Int(n) => Ok(Value::Int(*n)),
            Expr::BinOpAddSub {
                lhs,
                op: OpAddSub::Add,
                rhs,
            } => Ok((self.eval_to_int(lhs)? + self.eval_to_int(rhs)?).into()),
            Expr::BinOpAddSub {
                lhs,
                op: OpAddSub::Sub,
                rhs,
            } => Ok((self.eval_to_int(lhs)? - self.eval_to_int(rhs)?).into()),
            Expr::BinOpMul {
                lhs,
                op: OpMul::Mul,
                rhs,
            } => Ok((self.eval_to_int(lhs)? * self.eval_to_int(rhs)?).into()),
            _ => todo!(),
        }
    }

    pub fn eval_to_int(&mut self, expr: &Expr) -> Result<i32, EvalError> {
        match self.eval_expr(expr)? {
            Value::Int(n) => Ok(n),
            other => Err(EvalError::TypeError("Not int".into(), other)),
        }
    }
}
