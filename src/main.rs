use std::error::Error;

use borlang::{parse_expr, Env};
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

struct Repl {
    env: Env,
}

impl Repl {
    fn new() -> Repl {
        let mut repl = Repl { env: Env::new() };
        repl.env.allow_rebind_global(true);
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
            }
            Ok(ast) => match self.env.eval_expr(&ast, &None) {
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
