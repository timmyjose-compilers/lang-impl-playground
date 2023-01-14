use std::{convert::From, error::Error, fmt, num::ParseIntError};

#[derive(Debug, PartialEq)]
pub enum Lexeme {
    Lint(i32),
    Lsym(String),
    Lend,
}

#[derive(Debug)]
pub struct Lexer {
    src: String,
    pos: usize,
    size: usize,
}

impl Lexer {
    pub fn new(src: String) -> Self {
        let len = src.len();

        Lexer {
            src,
            pos: 0,
            size: len,
        }
    }

    fn forward(&mut self) {
        self.pos += 1;
    }

    fn forward_n(&mut self, n: usize) {
        self.pos += n;
    }

    fn extract<P>(&mut self, pred: P) -> Result<String, LexerError>
    where
        P: Fn(char) -> bool,
    {
        let mut init_pos = self.pos;
        let mut pos = init_pos;

        while pos < self.size && pred(self.src.chars().nth(pos).unwrap()) {
            pos += 1;
        }

        self.pos = pos;
        Ok(String::from(&self.src[init_pos..pos]))
    }

    fn extract_int(&mut self) -> Result<i32, LexerError> {
        Ok(self.extract(|c| c.is_digit(10))?.parse::<i32>()?)
    }

    fn lex_char(&mut self, c: char) -> Result<Lexeme, LexerError> {
        match c {
            ' ' | '\n' | '\t' => {
                self.forward();
                self.lex()
            }
            '0'..='9' => Ok(Lexeme::Lint(self.extract_int()?)),
            '+' | '-' | '*' | '/' | '%' => {
                self.forward();
                Ok(Lexeme::Lsym(c.to_string()))
            }

            _ => Err(LexerError::new(format!("Invalid character: {}", c))),
        }
    }

    pub fn lex(&mut self) -> Result<Lexeme, LexerError> {
        if self.pos >= self.size {
            Ok(Lexeme::Lend)
        } else {
            self.lex_char(self.src.chars().nth(self.pos).unwrap())
        }
    }
}

#[derive(Debug)]
pub struct LexerError {
    message: String,
}

impl LexerError {
    pub fn new(message: String) -> Self {
        LexerError { message }
    }
}

impl Error for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl From<ParseIntError> for LexerError {
    fn from(err: ParseIntError) -> Self {
        LexerError::new(err.to_string())
    }
}
