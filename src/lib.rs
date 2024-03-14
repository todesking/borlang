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

#[macro_export]
macro_rules! object_value {
    ($($name:ident : $value:expr),*) => {{
        #[allow(unused_mut)]
        let mut obj: ::std::collections::HashMap<$crate::ast::Ident, $crate::value::Value> = ::std::collections::HashMap::new();
        object_value!(@impl obj, $($name : $value),*)
    }};
    (@impl $obj:ident, $name:ident : $value:expr, $($rest_name:ident : $rest_value:expr),*) => {{
        $obj.insert($crate::ast::Ident(stringify!($name).to_owned()), $value.into());
        object_value!(@impl $obj, $($rest_name : $rest_value),*)
    }};
    (@impl $obj:ident, $name:ident : $value:expr) => {
        object_value!(@impl $obj, $name : $value,)
    };
    (@impl $obj:ident,) => {
        $crate::value::Value::object($obj)
    };
}

#[macro_export]
macro_rules! array_value {
    ($($values:expr),*) => {{
        #[allow(unused_mut)]
        let mut obj: ::std::vec::Vec<$crate::value::Value> = ::std::vec::Vec::new();
        array_value!(@impl obj, $($values),*)
    }};
    (@impl $obj:ident, $value:expr, $($values:expr),*) => {{
        $obj.push($value.into());
        array_value!(@impl $obj, $($values),*)
    }};
    (@impl $obj:ident, $value:expr) => {
        array_value!(@impl $obj, $value,)
    };
    (@impl $obj:ident,) => {
        $crate::value::Value::array($obj)
    };
}

#[cfg(test)]
mod test {
    use crate::evaluator::EvalError;

    use super::*;
    use pretty_assertions::assert_eq;

    #[ctor::ctor]
    fn before_all() {
        color_backtrace::install();
    }

    macro_rules! assert_eval_ok {
        ($expr:tt, $expected:expr) => {
            assert_eval_impl!($expr, $expected, unwrap);
        };
    }
    macro_rules! assert_eval_err {
        ($expr:tt, $expected:expr) => {
            assert_eval_impl!($expr, $expected, unwrap_err);
        };
    }
    macro_rules! assert_eval_impl {
        ($actual:literal, $expected:expr, $unwrap:ident) => {{
            assert_eval_impl!([$actual], $expected, $unwrap);
        }};
        ([$($es:literal),+], $expected:expr, $unwrap:ident) => {
            let mut env = Env::prelude();
            assert_eval_impl!(@env env, [$($es),+], $expected, $unwrap);
        };
        (@env $env:ident, [$e1:literal, $($es:literal),+], $expected:expr, $unwrap:ident) => {
            $env.eval_expr(&parse_expr($e1).unwrap(), &None).unwrap();
            assert_eval_impl!(@env $env, [$($es),+], $expected, $unwrap);
        };
        (@env $env:ident, [$e1:literal], $expected:expr, $unwrap:ident) => {
            assert_eq!($env.eval_expr(&parse_expr($e1).unwrap(), &None).$unwrap(), $expected.into());
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

    #[test]
    fn bool() {
        assert_eval_ok!("true", true);
        assert_eval_ok!("false", false);
    }

    #[test]
    fn op_eq() {
        assert_eval_ok!("1 == 1", true);
        assert_eval_ok!("1 == 2", false);
        assert_eval_ok!("1 != 2", true);
        assert_eval_ok!("1 != 1", false);
        assert_eval_ok!("1 + 2 * 3 == 3 + 4", true);
        assert_eval_ok!("1 + 2 * 3 != 3 + 4 - 1", true);
        assert_eval_ok!("1 == 1 == 1", false);
        assert_eval_ok!("1 == 1 == true", true);
    }

    #[test]
    fn if_else() {
        assert_eval_ok!("if 1 == 2 { 1 } else { 2 }", 2);
        assert_eval_ok!("if 1 != 2 { 1 } else { 2 }", 1);
        assert_eval_ok!(["let a = 1", "if true { a = 1 } else { a = 2 }", "a"], 1);
        assert_eval_ok!(["let a = 1", "if false { a = 1 } else { a = 2 }", "a"], 2);
    }

    #[test]
    fn obj() {
        assert_eval_ok!("{}", object_value! {});
        assert_eval_ok!("{foo: 1, bar: true}", object_value! {foo: 1, bar: true});
        assert_eval_ok!(
            "{foo: 1 + 2, bar: {}}",
            object_value! {foo: 3, bar: object_value!{}}
        );
    }
    #[test]
    fn obj_prop() {
        assert_eval_err!("{}.foo", EvalError::PropertyNotFound("foo".into()));
        assert_eval_ok!("{foo: 123}.foo", 123);
        assert_eval_ok!("{foo: 123, bar: {baz: 999}}.bar.baz", 999);
    }

    #[test]
    fn array() {
        assert_eval_ok!("[]", array_value![]);
        assert_eval_ok!(
            "[1, {}, []]",
            array_value![1, object_value! {}, array_value![]]
        );
    }
}
