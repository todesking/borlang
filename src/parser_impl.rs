use tree_sitter::Language;

extern "C" {
    fn tree_sitter_borlang_program() -> Language;
    fn tree_sitter_borlang_expr() -> Language;
}

pub fn language_program() -> Language {
    unsafe { tree_sitter_borlang_program() }
}

pub fn language_expr() -> Language {
    unsafe { tree_sitter_borlang_expr() }
}
