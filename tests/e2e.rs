use borlang::{module::LoadError, parse_program, parser::ParseError, EvalError};

fn panic_on_parse_error(path: &str, src: &str, err: &ParseError) -> ! {
    let locs = err.error_locations();
    if locs.is_empty() {
        panic!("{err}");
    }
    panic!(
        "Parse error: {err} at {path}\nloccations={:?}\n{}",
        err.error_locations(),
        err.highlight_error_locations(src)
    );
}

fn handle_eval_error(err: EvalError) -> ! {
    match err {
        EvalError::LoadError(LoadError::ParseError(path, src, err)) => {
            panic_on_parse_error(path.to_str().unwrap(), &src, &err)
        }
        _ => panic!("{err}"),
    }
}

#[test_generator::test_resources("e2e/**/test_*.borlang")]
fn test_e2e(src_path: &str) {
    let src = std::fs::read_to_string(src_path).unwrap();
    let program = parse_program(&src).unwrap_or_else(|e| panic_on_parse_error(src_path, &src, &e));

    let mut rt = borlang::RuntimeContext::with_paths(vec!["lib"])
        .unwrap_or_else(|err| handle_eval_error(err));
    let module = rt
        .new_module(borlang::module::ModulePath::new("__test__"))
        .unwrap_or_else(|err| handle_eval_error(err));

    rt.eval_program_in_module(&program, &module)
        .unwrap_or_else(|err| handle_eval_error(err));
}
