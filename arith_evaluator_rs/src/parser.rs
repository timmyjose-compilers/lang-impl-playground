use super::ast::{BinaryOp, Expression, UnaryOp};
use std::error::Error;
use std::fmt;

#[derive(Debug)]
enum StackElem {
    Texp(Box<Expression>),
    Tunr(UnaryOp),
    Tbin(BinaryOp),
    Tlp,
}

#[derive(Debug)]
pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Parser {}
    }

    fn priority_uop(op: &UnaryOp) -> i32 {
        match op {
            UnaryOp::Uplus | UnaryOp::Uminus => 4,
        }
    }

    fn priority_bop(op: &BinaryOp) -> i32 {
        match op {
            BinaryOp::Mod => 1,
            BinaryOp::Plus | BinaryOp::Minus => 2,
            BinaryOp::Mult | BinaryOp::Div => 3,
        }
    }

    fn uop_of_string(sym: &str) -> Result<UnaryOp, ParserError> {
        match sym {
            "+" => Ok(UnaryOp::Uplus),
            "-" => Ok(UnaryOp::Uminus),
            _ => Err(ParserError::new(format!(
                "invalid symbol for unary operator: {}",
                sym
            ))),
        }
    }

    fn bop_of_string(sym: &str) -> Result<BinaryOp, ParserError> {
        match sym {
            "+" => Ok(BinaryOp::Plus),
            "-" => Ok(BinaryOp::Minus),
            "*" => Ok(BinaryOp::Mult),
            "/" => Ok(BinaryOp::Div),
            "%" => Ok(BinaryOp::Mod),
            _ => Err(ParserError::new(format!(
                "invalid symbol for binary operator: {}",
                sym
            ))),
        }
    }

    fn tsym(sym: &str) -> Result<StackElem, ParserError> {
        match Parser::bop_of_string(sym) {
            Ok(bop) => Ok(StackElem::Tbin(bop)),
            Err(_) => Ok(StackElem::Tunr(Parser::uop_of_string(sym)?)),
        }
    }

    fn reduce(&self, priority: usize, st: Vec<StackElem>) {
        match st.as_slice() {
            _ => todo!(),
        }

        todo!()
    }
}

#[derive(Debug)]
struct ParserError {
    message: String,
}

impl ParserError {
    pub fn new(message: String) -> Self {
        ParserError { message }
    }
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
