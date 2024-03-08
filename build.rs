use std::{error::Error, path::Path, process::Command};

fn main() -> Result<(), Box<dyn Error>> {
    let parser_dir = Path::new("tree-sitter-borlang");

    build_parser(parser_dir, "program")?;
    build_parser(parser_dir, "expr")?;

    println!("cargo:rerun-if-changed={}", parser_dir.to_str().unwrap());

    Ok(())
}

fn build_parser(base_dir: &Path, grammar_name: &str) -> Result<(), Box<dyn Error>> {
    let parser_dir = base_dir.join(format!("{grammar_name}_parser"));

    std::fs::create_dir_all(&parser_dir)?;

    let grammar_path = base_dir.join(format!("{grammar_name}_grammar.js"));
    tree_sitter_cli_generate(&parser_dir, &grammar_path)?;

    tree_sitter_build(&parser_dir, grammar_name)?;

    Ok(())
}

fn tree_sitter_cli_generate(dir: &Path, grammar_path: &Path) -> Result<(), Box<dyn Error>> {
    let mut cmd = Command::new("npm");
    let cmd = cmd
        .current_dir(dir)
        .args(["exec", "--", "tree-sitter-cli", "generate", "--no-bindings"])
        .arg(&std::fs::canonicalize(grammar_path).unwrap());

    println!("tree-sitter-cli generate {:?}", &cmd.get_args());

    let out = cmd.output()?;
    if !out.status.success() {
        return Err(format!(
            "parser generation failed: stdout={}, stderr={}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        )
        .into());
    }

    Ok(())
}

fn tree_sitter_build(dir: &Path, grammar_name: &str) -> Result<(), Box<dyn Error>> {
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
    Ok(())
}
