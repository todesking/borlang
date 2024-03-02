use std::error::Error;

use rustyline::error::ReadlineError;

fn main() -> Result<(), Box<dyn Error>> {
    let ret = repl_main()?;
    std::process::exit(ret);
}

fn repl_main() -> Result<i32, Box<dyn Error>> {
    let mut rl = rustyline::DefaultEditor::new()?;

    loop {
        let line = rl.readline("borlang> ");
        match line {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                let ast = borlang::parser::parse(&line);
                println!("{:?}", ast);
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
