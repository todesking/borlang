use std::{error::Error};





use crate::{
    ast::{Expr, Program},
    parser_impl,
};

pub fn parse_program(src: &str) -> Result<Program, Box<dyn Error>> {
    let language = parser_impl::language_program();

    let mut ts = tree_sitter::Parser::new();
    ts.set_language(language)?;

    let tree = ts
        .parse(src, None)
        .expect("tree_sitter's parse() should return tree");

    serde_tree_sitter::from_tree(&tree, src, true).map_err(|e| e.into())
}

pub fn parse_expr(src: &str) -> Result<Expr, Box<dyn Error>> {
    let language = parser_impl::language_expr();

    let mut ts = tree_sitter::Parser::new();
    ts.set_language(language)?;

    let tree = ts
        .parse(src, None)
        .expect("tree_sitter's parse() should return tree");

    #[derive(serde::Deserialize)]
    #[serde(rename = "expr")]
    struct Wrapper((Expr,));

    serde_tree_sitter::from_tree::<Wrapper>(&tree, src, true)
        .map_err(|e| e.into())
        .map(|v| v.0 .0)
}
