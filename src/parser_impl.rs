use tree_sitter::Language;

extern "C" {
    fn tree_sitter_borlang() -> Language;
    fn tree_sitter_borlang_expr() -> Language;
}

pub fn language_program() -> Language {
    unsafe { tree_sitter_borlang() }
}

pub fn language_expr() -> Language {
    unsafe { tree_sitter_borlang_expr() }
}
