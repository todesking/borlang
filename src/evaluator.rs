use crate::Expr;
use crate::Program;
use crate::Value;
use std::error::Error;

pub struct Env {}

impl Env {
    pub fn prelude() -> Env {
        let mut env = Env{};
        env
    }
    pub fn eval(&mut self, program: &Program) -> Result<Value, Box<dyn Error>> {
        match program {
           Program::Expr(expr) => {
               match expr {
                   Expr::Int(n) => Ok(Value::Int(*n)),
                   _ => todo!()
               }
           }
        }
    }
}
