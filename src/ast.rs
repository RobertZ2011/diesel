#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Clone, Debug)]
pub enum Expr {
    ConstInt(i64),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Var(String),
    FunctionApp(String, Vec<Box<Expr>>),
    Block(Vec<Box<Expr>>)
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    FunctionDef(String, Vec<String>, Box<Expr>)
}