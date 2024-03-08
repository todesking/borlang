use std::error::Error;

use borlang::{parse, Env};
use rustyline::error::ReadlineError;

fn main() -> Result<(), Box<dyn Error>> {
    let ret = repl_main()?;
    std::process::exit(ret);
}

fn repl_main() -> Result<i32, Box<dyn Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;

    let mut env = Env::prelude();

    loop {
        let line = rl.readline("borlang> ");
        match line {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                let ast = parse(&line);
                println!("{:?}", ast);
                if let Ok(ast) = ast {
                    let value = env.eval_program(&ast);
                    dbg!(&value);
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
