use std::error::Error;

use borlang::{
    module::{FsModuleLoader, LoadError, Module, ModulePath},
    parse_expr,
    parser::ParseError,
    EvalError, RuntimeContext,
};
use gc::Gc;
use rustyline::error::ReadlineError;

fn main() -> Result<(), Box<dyn Error>> {
    let ret = repl_main()?;
    std::process::exit(ret);
}

fn repl_main() -> Result<i32, Box<dyn Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;
    let mut repl = Repl::new();

    loop {
        let line = rl.readline("borlang> ");
        match line {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                repl.handle_input(&line);
            }
            Err(ReadlineError::Eof) => {
                return Ok(0);
            }
            Err(ReadlineError::Interrupted) => {
                return Ok(1);
            }
            Err(e) => return Err(Box::new(e)),
        }
    }
}

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

fn handle_eval_error<T>(err: EvalError) -> T {
    match err {
        EvalError::LoadError(LoadError::ParseError(path, src, err)) => {
            panic_on_parse_error(path.to_str().unwrap(), &src, &err)
        }
        _ => panic!("{err}"),
    }
}

struct Repl {
    ctx: RuntimeContext<FsModuleLoader>,
    module: Gc<Module>,
}

impl Repl {
    fn new() -> Repl {
        let mut ctx = RuntimeContext::with_paths(vec!["lib"]).unwrap_or_else(handle_eval_error);
        let module = ctx.new_module(ModulePath::new("__repl__")).unwrap();
        let mut repl = Repl { ctx, module };
        repl.ctx.allow_rebind_global(true);
        repl
    }
    fn handle_input(&mut self, line: &str) {
        let line = line.trim();
        match line {
            "" => {}
            _ if line.starts_with(':') => {
                self.handle_command(line);
            }
            _ => {
                self.handle_code(line);
            }
        }
    }
    fn handle_code(&mut self, line: &str) {
        let ast = parse_expr(line);
        match ast {
            Err(err) => {
                println!("Parse error: {:?}", err);
                let errors = err.error_locations();
                if !errors.is_empty() {
                    println!("{}", err.highlight_error_locations(line));
                }
            }
            Ok(ast) => match self.ctx.eval_expr_in_module(&ast, &self.module) {
                Ok(value) => {
                    println!("=> {}", value);
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                }
            },
        }
    }

    fn handle_command(&self, line: &str) {
        match &line.splitn(2, ' ').collect::<Vec<_>>()[..] {
            [":ast", src] => {
                let ast = parse_expr(src);
                dbg!(&ast);
            }
            _ => {
                println!("Invalid command.");
            }
        }
    }
}
