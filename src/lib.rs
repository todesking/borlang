pub mod ast;
pub mod eval_error;
pub mod intrinsic;
pub mod module;
pub mod parser;
pub mod runtime;
pub mod value;

mod parser_impl;

pub use ast::Expr;
pub use ast::Program;
pub use eval_error::EvalError;
pub use parser::{parse_expr, parse_program};
pub use runtime::EvalResult;
pub use runtime::RuntimeContext;
pub use value::Value;

#[macro_export]
macro_rules! object_value {
    ($name:ident : $value:expr $(,$($rest:tt)*)?) => {{
        #[allow(unused_mut)]
        let mut obj = $crate::value::ObjectValue::new();
        object_value!(@impl obj, $name: $value, $($($rest)*)?)
    }};
    ([sym $name:literal]: $value:expr $(,$($rest:tt)*)?) => {{
        #[allow(unused_mut)]
        let mut obj = $crate::value::ObjectValue::new();
        object_value!(@impl obj, [sym $name]: $value, $($($rest)*)?)
    }};
    ($(,)?) => {{
        $crate::value::ObjectValue::new()
    }};
    (@impl $obj:ident, $name:ident : $value:expr $(,$($rest:tt)*)?) => {{
        $obj.insert($crate::value::ObjectKey::Str(std::rc::Rc::new(stringify!($name).to_owned())), $value.into());
        object_value!(@impl $obj, $($($rest)*)?)
    }};
    (@impl $obj:ident, [sym $name:literal] : $value:expr $(, $($rest:tt)*)?) => {{
        $obj.insert($crate::value::ObjectKey::Sym(std::rc::Rc::new($name.to_owned())), $value.into());
        object_value!(@impl $obj, $($($rest)*)?)
    }};
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
    use std::{path::PathBuf, rc::Rc};

    use crate::module::ModulePath;
    use crate::value::ObjectKey;

    use self::{
        module::{FsModuleLoader, LoadError, Module, ModuleLoader},
        parser::ParseError,
    };

    use super::*;
    use gc::Gc;
    use pretty_assertions::assert_eq;

    fn panic_on_parse_error(path: &str, src: &str, err: &ParseError) -> ! {
        let locs = err.error_locations();
        if locs.is_empty() {
            panic!("{err}");
        }
        panic!(
            "Parse error: {err} at {path}\nloccations={:?}\n{}",
            err.error_locations(),
            err.highlight_error_locations(src)
        );
    }

    fn handle_eval_error(err: EvalError) -> ! {
        match err {
            EvalError::LoadError(LoadError::ParseError(path, src, err)) => {
                panic_on_parse_error(path.to_str().unwrap(), &src, &err)
            }
            _ => panic!("{err}"),
        }
    }

    #[ctor::ctor]
    fn before_all() {
        color_backtrace::install();
    }

    fn parse_expr_or_die(src: &str) -> Expr {
        match parse_expr(src) {
            Ok(expr) => expr,
            Err(err) => parse_error(src, &err),
        }
    }
    fn parse_program_or_die(src: &str) -> Program {
        match parse_program(src) {
            Ok(expr) => expr,
            Err(err) => parse_error(src, &err),
        }
    }

    fn parse_error(src: &str, err: &ParseError) -> ! {
        let locs = err.error_locations();
        if locs.is_empty() {
            panic!("{err}");
        }
        panic!("Parse error {err}\n{}", err.highlight_error_locations(src));
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
            let mut ctx = RuntimeContext::with_paths(vec!["lib"]).unwrap_or_else(|err| handle_eval_error(err));
            let module = ctx.new_module(crate::module::ModulePath::new("__test__")).unwrap_or_else(|err| handle_eval_error(err));
            assert_eval_impl!(@ctx ctx, module, [$($es),+], $expected, $unwrap);
        };
        (@ctx $ctx:ident, $module:ident, [$e1:literal, $($es:literal),+], $expected:expr, $unwrap:ident) => {
            $ctx.eval_expr_in_module(&parse_expr_or_die($e1), &$module).unwrap_or_else(|err| handle_eval_error(err));
            assert_eval_impl!(@ctx $ctx, $module, [$($es),+], $expected, $unwrap);
        };
        (@ctx $ctx:ident, $module:ident, [$e1:literal], $expected:expr, $unwrap:ident) => {
            assert_eq!($ctx.eval_expr_in_module(&parse_expr_or_die($e1), &$module).$unwrap(), $expected.into());
        };
    }

    fn new_rt() -> (RuntimeContext<FsModuleLoader>, Gc<Module>) {
        let mut ctx =
            RuntimeContext::with_paths(vec!["lib"]).unwrap_or_else(|err| handle_eval_error(err));
        let module = ctx
            .new_module(crate::module::ModulePath::new("__test__"))
            .unwrap();
        (ctx, module)
    }

    fn eval_program<L: ModuleLoader>(
        rt: &mut RuntimeContext<L>,
        module: &Gc<Module>,
        src: &'static str,
    ) -> EvalResult<()> {
        let program = parse_program_or_die(src);
        rt.eval_program_in_module(&program, module)
    }

    fn eval_expr<L: ModuleLoader>(
        rt: &mut RuntimeContext<L>,
        module: &Gc<Module>,
        src: &'static str,
    ) -> EvalResult {
        let program = parse_expr_or_die(src);
        rt.eval_expr_in_module(&program, module)
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
    fn do_block() {
        assert_eval_ok!("do {1}", 1);
        assert_eval_ok!("do {1;}", Value::null());
        assert_eval_ok!("do {1; 2}", 2);
        assert_eval_ok!("do {1; 2;}", Value::null());
    }

    #[test]
    fn var() {
        assert_eval_err!("a", EvalError::NameNotFound("a".into()));
        assert_eval_ok!("do {let a = 123; let b = 456; a}", 123);
    }

    #[test]
    fn var_name_conflict() {
        // local
        assert_eval_err!(
            "do {let a = 1; let a = 2; a}",
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
        assert_eval_ok!("do {let a = 1; a = 2; a}", 2);
        assert_eval_ok!("do {let a = 1; do {a = 2}; a}", 2);
        assert_eval_ok!("do {let a = 1; do {let a = 999; a = 2}; a}", 1);

        assert_eval_err!("do {let a = 1; b = 2}", EvalError::NameNotFound("b".into()));
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
    fn object_assign() {
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
    fn object_assign_dyn() {
        let (mut rt, m) = new_rt();
        eval_program(
            &mut rt,
            &m,
            r#"
            sym s;
            let o = {a: 1};
        "#,
        )
        .unwrap();
        assert_eq!(
            eval_expr(&mut rt, &m, r#"do { o.["a"] = 2; o.[s] = 99; o}"#),
            Ok(object_value! {a: 2, [sym "__test__:s"]: 99}.into())
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
        assert_eval_ok!("do {let f = fn() => 123; f()}", 123);
        assert_eval_ok!("(fn(x) => x + 1)(100)", 101);
        assert_eval_ok!(
            "do {
                let f = fn(x) => fn(y) => x + y;
                let add1 = f(1);
                add1(99)
            }",
            100
        );
        assert_eval_ok!("do { let x = 10; (fn() => x)() }", 10);
        assert_eval_ok!("do { let x = 10; (fn(x) => x)(20) }", 20);
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
    fn bool_not() {
        assert_eval_ok!("!true", false);
        assert_eval_ok!("!false", true);
        assert_eval_err!(
            "!0",
            EvalError::property_not_found(ObjectKey::Sym(Rc::new("_internal:op_not".into())))
        );
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
            EvalError::property_not_found(ObjectKey::Sym(Rc::new(
                "_internal:iterable_iterator".to_owned()
            )))
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
    fn obj_prop_dyn_str() {
        assert_eval_ok!(r#"{a: 1}.["a"]"#, 1);
    }

    #[test]
    fn obj_prop_dyn_sym() {
        let (mut rt, m) = new_rt();
        eval_program(&mut rt, &m, "sym a; let o = {[a]: 1};").unwrap();
        assert_eq!(eval_expr(&mut rt, &m, "o.[a]").unwrap(), Value::int(1),);
    }

    #[test]
    fn obj_literal_dyn_key() {
        let (mut rt, m) = new_rt();
        eval_program(&mut rt, &m, "sym a; let o = {[a]: 1};").unwrap();
        assert_eq!(
            eval_expr(&mut rt, &m, "o").unwrap(),
            Value::object(object_value! { [sym "__test__:a"]: 1})
        );
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
        rt.eval_expr_in_module(&parse_expr_or_die("let a = 1"), &m)
            .unwrap();
        assert_eq!(
            rt.eval_expr_in_module(&parse_expr_or_die("let a = 1"), &m),
            Err(EvalError::NameDefined("a".into()))
        );
        rt.allow_rebind_global(true);
        assert_eq!(
            rt.eval_expr_in_module(&parse_expr_or_die("let a = 1"), &m),
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
    fn op_negat() {
        assert_eval_ok!("-1", -1);
        assert_eval_ok!("1 + -[1,2,3][1] * 3", -5);
        assert_eval_err!(
            "-[]",
            EvalError::property_not_found(ObjectKey::Sym(Rc::new("_internal:op_negate".into())))
        );
    }

    #[test]
    fn top_term_let() {
        let mut rt = RuntimeContext::load(FsModuleLoader::new(vec![PathBuf::from("lib")]))
            .unwrap_or_else(|err| handle_eval_error(err));
        let m = rt.new_module(ModulePath::new("__test__")).unwrap();
        rt.eval_program_in_module(&parse_program("pub let a = 1; let b = 2;").unwrap(), &m)
            .unwrap();
        assert_eq!(
            rt.eval_expr_in_module(&parse_expr_or_die("a"), &m),
            Ok(Value::int(1))
        );
        assert_eq!(
            rt.eval_expr_in_module(&parse_expr_or_die("b"), &m),
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
            rt.eval_expr_in_module(&parse_expr_or_die("prelude.true"), &m)
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

    #[test]
    fn top_term_sym() {
        let (mut rt, m) = new_rt();

        eval_program(
            &mut rt,
            &m,
            r#"
        sym private_sym;
        pub sym public_sym;
        "#,
        )
        .unwrap();

        assert_eq!(
            eval_expr(&mut rt, &m, "private_sym"),
            Ok(Value::sym("__test__:private_sym"))
        );
        assert_eq!(
            eval_expr(&mut rt, &m, "public_sym"),
            Ok(Value::sym("__test__:public_sym"))
        );

        assert_eq!(
            m.pub_object(),
            &Value::object(object_value! {public_sym: Value::sym("__test__:public_sym")})
        );
    }

    #[test]
    fn throw() {
        assert_eval_err!("throw(123)", EvalError::Exception { data: 123.into() });
    }

    #[test]
    fn catch() {
        assert_eval_ok!(
            [
                "let err = fn(e) => throw(e)",
                "do { err(123) } catch e => e"
            ],
            123
        );
    }
}
