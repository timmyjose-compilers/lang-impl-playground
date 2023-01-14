use std::error::Error;
use std::fmt;

#[derive(Debug)]
struct EvalError {
    message: String,
}

impl EvalError {
    pub fn new(message: &str) -> Self {
        EvalError {
            message: message.to_owned(),
        }
    }
}

impl Error for EvalError {}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
