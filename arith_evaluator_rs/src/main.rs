use arith_evaluator_rs::lexer::{Lexeme, Lexer};

use std::{
    error::Error,
    io::{self, Write},
};

const PROMPT: &'static str = ">> ";

fn flush() -> Result<(), Box<dyn Error>> {
    io::stdout().lock().flush()?;
    Ok(())
}

fn read_line() -> Result<String, Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    Ok(input.trim().to_owned())
}

fn main() -> Result<(), Box<dyn Error>> {
    println!("Welcome to the arithmetic expression parser evaluator (<Ctrl+C> to quit)");

    loop {
        print!("{}", PROMPT);
        flush()?;
        let input = read_line()?;
        let mut lexer = Lexer::new(input);
        loop {
            match lexer.lex() {
                Err(e) => eprintln!("{:?}", e),
                Ok(lexeme) => {
                    println!("{:?}", lexeme);
                    if lexeme == Lexeme::Lend {
                        break;
                    }
                }
            }
        }
    }
}
