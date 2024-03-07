pub mod ast;
pub mod evaluator;
pub mod parser;
pub mod value;

mod parser_impl;

pub use ast::Expr;
pub use ast::Program;
pub use evaluator::Env;
pub use parser::parse;
pub use value::Value;

#[cfg(test)]
mod test {
    use crate::evaluator::EvalError;

    use super::*;
    use pretty_assertions::assert_eq;

    #[ctor::ctor]
    fn before_all() {
        color_backtrace::install();
    }

    fn eval(s: &str) -> Result<Value, EvalError> {
        let src = format!("let actual = {};", s);
        let tree = parse(&src).unwrap();

        let mut env = Env::prelude();
        env.eval_program(&tree)?;
        env.get_var("actual")
    }

    macro_rules! assert_eval_ok {
        ($actual:literal, $expected:expr) => {
            assert_eq!(eval($actual).unwrap(), $expected.into());
        };
    }
    macro_rules! assert_eval_err {
        ($actual:literal, $expected:expr) => {
            assert_eq!(eval($actual).unwrap_err(), $expected.into());
        };
    }

    #[test]
    fn int_lit() {
        assert_eval_ok!("1", 1);
    }
    #[test]
    fn add_sub() {
        assert_eval_ok!("1 + 1", 2);
        assert_eval_ok!("1 +1", 2);
        assert_eval_ok!("1+1", 2);
        assert_eval_ok!("10 - 1", 9);
        assert_eval_ok!("10 - 1 + 3", 12);
    }

    #[test]
    fn mul() {
        assert_eval_ok!("3 * 4", 12);
        assert_eval_ok!("3*4", 12);
    }

    #[test]
    fn block() {
        assert_eval_ok!("{}", Value::Null);
        assert_eval_ok!("{1}", 1);
        assert_eval_ok!("{1;}", Value::Null);
        assert_eval_ok!("{1; 2}", 2);
        assert_eval_ok!("{1; 2;}", Value::Null);
    }

    #[test]
    fn var() {
        assert_eval_err!("a", EvalError::NameNotFound("a".into()));
        assert_eval_ok!("{let a = 123; let b = 456; a}", 123);
    }
}
