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
    use std::error::Error;

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
}
