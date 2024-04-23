#[test_generator::test_resources("e2e/**/test_*.borlang")]
fn test_e2e(src_path: &str) {
    let src = std::fs::read_to_string(src_path).unwrap();
    let program = borlang::parse_program(&src).unwrap();

    let mut rt = borlang::RuntimeContext::with_paths(vec!["lib"]);
    let module = rt
        .new_module(borlang::module::ModulePath::new("__test__"))
        .unwrap();

    rt.eval_program_in_module(&program, &module).unwrap();
}
