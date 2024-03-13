pub mod ast;
pub mod evaluator;
pub mod parser;
pub mod value;

mod parser_impl;

pub use ast::Expr;
pub use ast::Program;
pub use evaluator::Env;
pub use parser::{parse_expr, parse_program};
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

    fn eval(src: &str) -> Result<Value, EvalError> {
        let tree = parse_expr(src).unwrap();

        let mut env = Env::prelude();
        env.eval_expr(&tree, &None)
    }

    macro_rules! assert_eval_ok {
        ($actual:literal, $expected:expr) => {
            assert_eq!(eval($actual).unwrap(), $expected.into());
        };
    }
    macro_rules! assert_eval_err {
        ($actual:literal, $expected:expr) => {
            assert_eval_err!([$actual], $expected);
        };
        ([$($es:literal),+], $expected:expr) => {
            let mut env = Env::prelude();
            assert_eval_err!(@env env, [$($es),+], $expected);
        };
        (@env $env:ident, [$e1:literal, $($es:literal),+], $expected:expr) => {
            $env.eval_expr(&parse_expr($e1).unwrap(), &None).unwrap();
            assert_eval_err!(@env $env, [$($es),+], $expected);
        };
        (@env $env:ident, [$e1:literal], $expected:expr) => {
            assert_eq!($env.eval_expr(&parse_expr($e1).unwrap(), &None).unwrap_err(), $expected.into());
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
    fn add_mul() {
        assert_eval_ok!("1 + 2 * 3", 7);
    }

    #[test]
    fn paren() {
        assert_eval_ok!("(1)", 1);
        assert_eval_ok!("(1 + 1)", 2);
        assert_eval_ok!("2 * (3 + 4)", 14);
    }

    #[test]
    fn block() {
        assert_eval_ok!("{}", Value::null());
        assert_eval_ok!("{1}", 1);
        assert_eval_ok!("{1;}", Value::null());
        assert_eval_ok!("{1; 2}", 2);
        assert_eval_ok!("{1; 2;}", Value::null());
    }

    #[test]
    fn var() {
        assert_eval_err!("a", EvalError::NameNotFound("a".into()));
        assert_eval_ok!("{let a = 123; let b = 456; a}", 123);
    }

    #[test]
    fn var_name_conflict() {
        // local
        assert_eval_err!(
            "{let a = 1; let a = 2; a}",
            EvalError::NameDefined("a".into())
        );
        // global
        assert_eval_err!(
            ["let a = 1", "let a = a"],
            EvalError::NameDefined("a".into())
        );
    }

    #[test]
    fn var_reassign() {
        assert_eval_ok!("{let a = 1; a = 2; a}", 2);
        assert_eval_ok!("{let a = 1; {a = 2}; a}", 2);
        assert_eval_ok!("{let a = 1; {let a = 999; a = 2}; a}", 1);

        assert_eval_err!("{let a = 1; b = 2}", EvalError::NameNotFound("b".into()));
        assert_eval_err!(["let a = 1", "b = 2"], EvalError::NameNotFound("b".into()));
    }

    #[test]
    fn function() {
        assert_eval_ok!("{let f = fn() => 123; f()}", 123);
        assert_eval_ok!("(fn(x) => x + 1)(100)", 101);
        assert_eval_ok!(
            "{
                let f = fn(x) => fn(y) => x + y;
                let add1 = f(1);
                add1(99)
            }",
            100
        );
        assert_eval_ok!("{ let x = 10; (fn() => x)() }", 10);
        assert_eval_ok!("{ let x = 10; (fn(x) => x)(20) }", 20);
        assert_eval_ok!("(fn(x,y,) => x + y)(1,2)", 3);
        assert_eval_ok!("(fn() => 42)()", 42);
        assert_eval_err!(
            "(fn() => 42)(123)",
            EvalError::ArgumentLength {
                expected: 0,
                actual: 1
            }
        );
    }
}
