pub mod ast;
pub mod eval_error;
pub mod evaluator;
pub mod intrinsic;
pub mod module;
pub mod parser;
pub mod value;

mod parser_impl;

pub use ast::Expr;
pub use ast::Program;
pub use eval_error::EvalError;
pub use evaluator::EvalResult;
pub use evaluator::RuntimeContext;
pub use parser::{parse_expr, parse_program};
pub use value::Value;

#[macro_export]
macro_rules! object_value {
    ($($name:ident : $value:expr),*) => {{
        #[allow(unused_mut)]
        let mut obj = $crate::value::ObjectValue::new();
        object_value!(@impl obj, $($name : $value),*)
    }};
    (@impl $obj:ident, $name:ident : $value:expr, $($rest_name:ident : $rest_value:expr),*) => {{
        $obj.insert($crate::value::ObjectKey::Str(std::rc::Rc::new(stringify!($name).to_owned())), $value.into());
        object_value!(@impl $obj, $($rest_name : $rest_value),*)
    }};
    (@impl $obj:ident, $name:ident : $value:expr) => {
        object_value!(@impl $obj, $name : $value,)
    };
    (@impl $obj:ident,) => {
        $obj
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
    use std::path::PathBuf;

    use crate::module::ModulePath;
    use crate::value::ObjectKey;

    use self::module::{FsModuleLoader, Module};

    use super::*;
    use gc::Gc;
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
            let mut ctx = RuntimeContext::with_paths(vec!["lib"]);
            let module = ctx.new_module(crate::module::ModulePath::new("__test__")).unwrap();
            assert_eval_impl!(@ctx ctx, module, [$($es),+], $expected, $unwrap);
        };
        (@ctx $ctx:ident, $module:ident, [$e1:literal, $($es:literal),+], $expected:expr, $unwrap:ident) => {
            $ctx.eval_expr_in_module(&parse_expr($e1).unwrap(), &$module).unwrap();
            assert_eval_impl!(@ctx $ctx, $module, [$($es),+], $expected, $unwrap);
        };
        (@ctx $ctx:ident, $module:ident, [$e1:literal], $expected:expr, $unwrap:ident) => {
            assert_eq!($ctx.eval_expr_in_module(&parse_expr($e1).unwrap(), &$module).$unwrap(), $expected.into());
        };
    }

    fn new_rt() -> (RuntimeContext<FsModuleLoader>, Gc<Module>) {
        let mut ctx = RuntimeContext::with_paths(vec!["lib"]);
        let module = ctx
            .new_module(crate::module::ModulePath::new("__test__"))
            .unwrap();
        (ctx, module)
    }

    #[test]
    fn int_lit() {
        assert_eval_ok!("1", 1);
        assert_eval_ok!("null", Value::null());
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
    fn int_compare() {
        assert_eval_ok!("1 < 2", true);
        assert_eval_ok!("2 < 1", false);
        assert_eval_ok!("1 <= 1", true);
        assert_eval_ok!("2 <= 1", false);
        assert_eval_ok!("2 > 1", true);
        assert_eval_ok!("1 > 2", false);
        assert_eval_ok!("1 >= 1", true);
        assert_eval_ok!("1 >= 2", false);
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
    fn array_reassign() {
        assert_eval_ok!(
            ["let a = [0, 1, 2]", "a[1] = 99", "a"],
            array_value![0, 99, 2]
        );
        assert_eval_err!(
            ["let a = [0, 1, 2]", "a[3] = 99"],
            EvalError::IndexOutOfBound { len: 3, index: 3 }
        );
        assert_eval_err!(
            ["let a = [0, 1, 2]", "a[n] = 99"],
            EvalError::name_not_found("n")
        );
        assert_eval_err!(
            ["let a = 0", "a[0] = 99"],
            EvalError::type_error("Array", 0)
        );
    }

    #[test]
    fn object_reassign() {
        assert_eval_ok!(["let o = {}", "o.foo = 1", "o"], object_value! {foo: 1});
        assert_eval_ok!(
            ["let o = {foo: 1}", "o.foo = 2", "o"],
            object_value! {foo: 2}
        );
        assert_eval_ok!(
            ["let o = {foo: 1, bar: 2}", "o.foo = 99", "o"],
            object_value! {
                foo: 99,
                bar: 2
            }
        );
        assert_eval_err!(
            ["let o = 0", "o.foo = 1"],
            EvalError::type_error("Object", 0)
        );
    }

    #[test]
    fn obj_spread() {
        assert_eval_ok!(
            "{a: 1, ..{}, b: 2, ..{}, ..{a: 11, c: 13}}",
            object_value! {a: 11, b: 2, c: 13}
        );
        assert_eval_err!("{..[]}", EvalError::type_error("Object", array_value![]));
    }

    #[test]
    fn obj_shorthand() {
        assert_eval_ok!(["let a = 1", "{a, b: 2}"], object_value! {a: 1, b: 2});
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
    fn test_for() {
        assert_eval_ok!(["let a = 0", "for x in [1, 2, 3] { a = a + x }", "a"], 6);
        assert_eval_err!(
            "for x in 0 {}",
            EvalError::property_not_found(ObjectKey::new_str_from_str("iterator"))
        );
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
        assert_eval_err!(
            "{}.foo",
            EvalError::PropertyNotFound(ObjectKey::new_str_from_str("foo"))
        );
        assert_eval_ok!("{foo: 123}.foo", 123);
        assert_eval_ok!("{foo: 123, bar: {baz: 999}}.bar.baz", 999);
    }

    #[test]
    fn optional_chain() {
        assert_eval_ok!("null?.foo", Value::null());
        assert_eval_ok!("{foo: 1}?.foo", 1);
    }

    #[test]
    fn array() {
        assert_eval_ok!("[]", array_value![]);
        assert_eval_ok!(
            "[1, {}, []]",
            array_value![1, object_value! {}, array_value![]]
        );
    }

    #[test]
    fn rebind_global() {
        let (mut rt, m) = new_rt();
        rt.eval_expr_in_module(&parse_expr("let a = 1").unwrap(), &m)
            .unwrap();
        assert_eq!(
            rt.eval_expr_in_module(&parse_expr("let a = 1").unwrap(), &m),
            Err(EvalError::NameDefined("a".into()))
        );
        rt.allow_rebind_global(true);
        assert_eq!(
            rt.eval_expr_in_module(&parse_expr("let a = 1").unwrap(), &m),
            Ok(Value::null()),
        );
    }

    #[test]
    fn string() {
        assert_eval_ok!(r#""""#, "");
        assert_eval_ok!(r#""foo""#, "foo");
        assert_eval_ok!(r#""HELLO\"WORLD""#, "HELLO\"WORLD");
    }

    #[test]
    fn array_len() {
        assert_eval_ok!("[].len()", 0);
        assert_eval_ok!("[1,2,3].len()", 3);
    }

    #[test]
    fn array_push() {
        assert_eval_ok!(
            ["let arr = [1,2,3]", "arr.push(4)", "arr"],
            array_value![1, 2, 3, 4]
        );
    }

    #[test]
    fn array_index() {
        assert_eval_ok!("[1,2,3][0]", 1);
        assert_eval_ok!("[1,[2,3],4][1][0]", 2);

        assert_eval_err!("[][[]]", EvalError::type_error("Int", array_value![]));
        assert_eval_err!("[][0]", EvalError::IndexOutOfBound { len: 0, index: 0 });
        assert_eval_err!("[][-1]", EvalError::cast_error("usize", -1));
    }

    #[test]
    fn array_spread() {
        assert_eval_ok!(
            "[1, ..[], ..[2], 3, ..[4, 5], 6]",
            array_value![1, 2, 3, 4, 5, 6]
        );
        assert_eval_err!("[..0]", EvalError::type_error("Array", 0));
    }

    #[test]
    fn op_mod() {
        assert_eval_ok!("10 % 5", 0);
        assert_eval_ok!("13 % 5", 3);
    }

    #[test]
    fn op_unary_minus() {
        assert_eval_ok!("-1", -1);
        assert_eval_ok!("1 + -[1,2,3][1] * 3", -5);
        assert_eval_err!("-[]", EvalError::type_error("Int", array_value![]));
    }

    #[test]
    fn top_term_let() {
        let mut rt = RuntimeContext::new(FsModuleLoader::new(vec![PathBuf::from("lib")]));
        let m = rt.new_module(ModulePath::new("__test__")).unwrap();
        rt.eval_program_in_module(&parse_program("pub let a = 1; let b = 2;").unwrap(), &m)
            .unwrap();
        assert_eq!(
            rt.eval_expr_in_module(&parse_expr("a").unwrap(), &m),
            Ok(Value::int(1))
        );
        assert_eq!(
            rt.eval_expr_in_module(&parse_expr("b").unwrap(), &m),
            Ok(Value::int(2))
        );
        assert_eq!(m.pub_object(), &Value::from(object_value! {a: 1}));
    }

    #[test]
    fn import() {
        let (mut rt, m) = new_rt();

        rt.eval_program_in_module(
            &parse_program(r#"let prelude = import "std/prelude";"#).unwrap(),
            &m,
        )
        .unwrap();
        assert_eq!(
            rt.eval_expr_in_module(&parse_expr("prelude.true").unwrap(), &m)
                .unwrap(),
            Value::bool(true)
        );
    }

    #[test]
    fn let_obj() {
        assert_eval_ok!(["let o = {a: 1, b: 2, c: 3}", "let {} = o"], Value::null());
        assert_eval_ok!(
            ["let o = {a: 1, b: 2, c: 3}", "let {a, b} = o", "[a, b]"],
            array_value![1, 2]
        );
        assert_eval_ok!(
            [
                "let o = {a: 1, b: 2, c: 3}",
                "let {a, ..rest} = o",
                "[a, rest]"
            ],
            array_value![1, object_value! {b: 2, c: 3}]
        );
        assert_eval_ok!(
            ["let o = {a: 1, b: 2, c: 3}", "let {..rest} = {}", "rest"],
            object_value! {}
        );
        assert_eval_err!(
            ["let o = {a: 1, b: 2, c: 3}", "let {z} = o"],
            EvalError::property_not_found(ObjectKey::new_str_from_str("z"))
        );
        assert_eval_err!("let {} = 1", EvalError::type_error("Object", 1));
    }
    #[test]
    fn let_arr() {
        assert_eval_ok!("let [] = []", Value::null());
        assert_eval_ok!(["let [a, b] = [1, 2]", "[b, a]"], array_value![2, 1]);
        assert_eval_err!(["let [] = [1, 2]"], EvalError::array_length(0, 2));
        assert_eval_err!("let [] = 1", EvalError::type_error("Array", 1));
        assert_eval_ok!(["let [..rest] = [1, 2]", "rest"], array_value![1, 2]);
        assert_eval_ok!(
            ["let [a, b, ..rest] = [1, 2, 3, 4]", "[a, b, rest]"],
            array_value![1, 2, array_value![3, 4]]
        );
    }
}
