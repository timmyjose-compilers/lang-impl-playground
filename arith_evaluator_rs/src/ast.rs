#[derive(Debug)]
pub enum UnaryOp {
    Uminus,
    Uplus,
}

#[derive(Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum Expression {
    ExpInt(i32),
    ExpUnary {
        op: UnaryOp,
        exp: Box<Expression>,
    },
    ExpBinary {
        lhs: Box<Expression>,
        op: BinaryOp,
        rhs: Box<Expression>,
    },
}
