use std::{os::unix::ffi::OsStrExt, path::Path, process::Command};

fn main() {
    let program_parser_dir = Path::new("tree-sitter-borlang");
    let expr_parser_dir = Path::new("tree-sitter-borlang-expr");

    build_parser(program_parser_dir, "program");
    build_parser(expr_parser_dir, "expr");

    rerun_if_js_changed_in(program_parser_dir);
    rerun_if_js_changed_in(expr_parser_dir);
}

fn rerun_if_js_changed_in(dir: &Path) {
    for f in std::fs::read_dir(dir).unwrap() {
        let f = f.unwrap();
        if f.path().extension().and_then(|x| x.to_str()) == Some("js") {
            println!("cargo:rerun-if-changed={}", f.path().to_str().unwrap());
        }
    }
}

fn build_parser(dir: &Path, grammar_name: &str) {
    tree_sitter_cli_generate(dir);
    tree_sitter_build(dir, grammar_name);
}

fn tree_sitter_cli_generate(dir: &Path) {
    let mut cmd = Command::new("npm");
    let cmd = cmd
        .current_dir(dir)
        .args(["exec", "--", "tree-sitter-cli", "generate", "--no-bindings"])
        .arg("grammar.js");

    println!("tree-sitter-cli generate {:?}", &cmd.get_args());

    let out = cmd.output().unwrap();
    println!("{}", String::from_utf8_lossy(&out.stdout));
    eprintln!("{}", String::from_utf8_lossy(&out.stderr));
    if !out.status.success() {
        panic!(
            "Parser generation failed: dir={}",
            String::from_utf8_lossy(dir.as_os_str().as_bytes())
        );
    }
}

fn tree_sitter_build(dir: &Path, grammar_name: &str) {
    let src_dir = dir.join("src");
    let mut c_config = cc::Build::new();
    c_config.include(&src_dir);
    c_config
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-unused-but-set-variable")
        .flag_if_supported("-Wno-trigraphs");
    let parser_path = src_dir.join("parser.c");
    c_config.file(&parser_path);
    c_config.compile(&format!("{grammar_name}_parser"));
}
