use crate::{
    ast::{Expr, Program},
    parser_impl,
};

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ParseError {
    #[error(transparent)]
    TreeSitterError(tree_sitter::LanguageError),
    #[error(transparent)]
    SerdeError(serde_tree_sitter::DeserializeError),
}

pub fn parse_program(src: &str) -> Result<Program, ParseError> {
    let language = parser_impl::language_program();

    let mut ts = tree_sitter::Parser::new();
    ts.set_language(language)
        .map_err(ParseError::TreeSitterError)?;

    let tree = ts
        .parse(src, None)
        .expect("tree_sitter's parse() should return tree");

    serde_tree_sitter::from_tree(&tree, src, true).map_err(ParseError::SerdeError)
}

pub fn parse_expr(src: &str) -> Result<Expr, ParseError> {
    let language = parser_impl::language_expr();

    let mut ts = tree_sitter::Parser::new();
    ts.set_language(language)
        .map_err(ParseError::TreeSitterError)?;

    let tree = ts
        .parse(src, None)
        .expect("tree_sitter's parse() should return tree");

    serde_tree_sitter::from_tree::<(Expr,)>(&tree, src, true)
        .map_err(ParseError::SerdeError)
        .map(|v| v.0)
}
