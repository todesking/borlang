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
impl ParseError {
    pub fn error_locations(&self) -> Vec<tree_sitter::Range> {
        match self {
            Self::SerdeError(serde_tree_sitter::DeserializeError::TreeSitterError(ranges)) => {
                ranges.clone()
            }
            _ => vec![],
        }
    }
    pub fn highlight_error_locations(&self, src: &str) -> String {
        let errors = self.error_locations();
        let mut out = String::new();
        let lines = src.split('\n');
        for (l, row) in lines.zip(0..) {
            out.push_str(&format!("{:>04} ", row + 1));
            out.push_str(l);
            out.push('\n');
            let mut annot = String::new();
            for (_, col) in l.chars().zip(0..) {
                let has_error = errors.iter().any(|e| {
                    let after_start = (e.start_point.row == row && e.start_point.column <= col)
                        || (e.start_point.row < row);
                    let before_end = (row == e.end_point.row && col <= e.end_point.column)
                        || (e.end_point.row > row);
                    after_start && before_end
                });
                if has_error {
                    annot.push('~');
                } else {
                    annot.push(' ');
                }
            }
            if !annot.chars().all(|c| c == ' ') {
                out.push_str("     ");
                out.push_str(&annot);
                out.push('\n');
            }
        }
        out
    }
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
