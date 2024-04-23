use borlang::{module::LoadError, parse_program, parser::ParseError, EvalError};

fn parse_error(src: &str, err: &ParseError) -> ! {
    let locs = err.error_locations();
    if locs.is_empty() {
        panic!("{err}");
    }
    panic!("Parse error {err}\n{}", err.highlight_error_locations(src));
}

#[test_generator::test_resources("e2e/**/test_*.borlang")]
fn test_e2e(src_path: &str) {
    let src = std::fs::read_to_string(src_path).unwrap();
    let program = parse_program(&src).unwrap_or_else(|e| parse_error(&src, &e));

    let mut rt = borlang::RuntimeContext::with_paths(vec!["lib"]);
    let module = rt
        .new_module(borlang::module::ModulePath::new("__test__"))
        .unwrap();

    match rt.eval_program_in_module(&program, &module) {
        Err(EvalError::LoadError(LoadError::ParseError(_path, err))) => {
            parse_error(&src, &err);
        }
        x => x.unwrap(),
    }
}
