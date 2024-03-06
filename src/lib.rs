pub mod evaluator;
pub mod parser;
pub mod syntax;
pub mod value;

pub use evaluator::Env;
pub use parser::parse;
pub use syntax::Expr;
pub use syntax::Program;
pub use value::Value;

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    #[ctor::ctor]
    fn before_all() {
        color_backtrace::install();
    }

    fn eval(s: &str) -> Result<Value, Box<dyn Error>> {
        let tree = parse(s)?;
        let mut env = Env::prelude();
        let value = env.eval(&tree)?;
        Ok(value)
    }

    macro_rules! assert_eval_ok {
        ($actual:literal, $expected:expr) => {
            assert_eq!(eval($actual).unwrap(), $expected.into());
        };
    }

    #[test]
    fn int_lit() {
        assert_eval_ok!("1", 1);
    }
    #[test]
    fn add_sub() {
        assert_eval_ok!("1 +1", 2);
        assert_eval_ok!("10 - 1", 9);
        assert_eval_ok!("10 - 1 + 3", 12);
    }

    #[test]
    fn mul() {
        assert_eval_ok!("3 * 4", 12);
    }
}
