use std::error::Error;

use crate::syntax::grammar::Program;

pub fn parse(src: &str) -> Result<Program, Box<dyn Error>> {
    crate::syntax::parse(src).map_err(|es| {
        es.into_iter()
            .map(|e| format!("{:?}", e))
            .collect::<Vec<_>>()
            .join("\n")
            .into()
    })
}
