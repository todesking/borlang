use std::{error::Error, path::Path, process::Command};

fn main() -> Result<(), Box<dyn Error>> {
    let parser_dir = Path::new("tree-sitter-borlang");

    let out = Command::new("npm")
        .current_dir(parser_dir)
        .args(["exec", "--", "tree-sitter-cli", "generate", "--no-bindings"])
        .output()?;
    if !out.status.success() {
        return Err(format!(
            "parser generation failed: stdout={}, stderr={}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        )
        .into());
    }

    let src_dir = parser_dir.join("src");
    let mut c_config = cc::Build::new();
    c_config.include(&src_dir);
    c_config
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-unused-but-set-variable")
        .flag_if_supported("-Wno-trigraphs");
    let parser_path = src_dir.join("parser.c");
    c_config.file(&parser_path);
    c_config.compile("parser");

    let grammar_dir = parser_dir.join("grammar.js");
    println!("cargo:rerun-if-changed={}", grammar_dir.to_str().unwrap());

    Ok(())
}
