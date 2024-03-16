use std::error::Error;

use borlang::{parse_expr, Env};
use rustyline::error::ReadlineError;

fn main() -> Result<(), Box<dyn Error>> {
    let ret = repl_main()?;
    std::process::exit(ret);
}

fn repl_main() -> Result<i32, Box<dyn Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;

    let mut env = Env::new();
    env.allow_rebind_global(true);

    loop {
        let line = rl.readline("borlang> ");
        match line {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                let ast = parse_expr(&line);
                match ast {
                    Err(err) => {
                        println!("Parse error: {:?}", err);
                    }
                    Ok(ast) => match env.eval_expr(&ast, &None) {
                        Ok(value) => {
                            println!("=> {}", value);
                        }
                        Err(err) => {
                            println!("Error: {:?}", err);
                        }
                    },
                }
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
