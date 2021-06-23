use crate::parser::token::Op;

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Gt,
    Lt,
    Gte,
    Lte
}

impl BinOp {
    pub fn from_op(op: Op) -> BinOp {
        match op {
            Op::Add => BinOp::Add,
            Op::Sub => BinOp::Sub,
            Op::Mul => BinOp::Mul,
            Op::Div => BinOp::Div,
            Op::Mod => BinOp::Mod,
            Op::Eq => BinOp::Eq,
            Op::Ne => BinOp::Ne,
            Op::Gt => BinOp::Gt,
            Op::Lt => BinOp::Lt,
            Op::Gte => BinOp::Gte,
            Op::Lte => BinOp::Lte,
            _ => panic!("from_op:: attemp to create BinOp from non-binary Op")
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Deref
}

impl UnaryOp {
    pub fn from_op(op: Op) -> UnaryOp {
        match op {
            Op::Deref => UnaryOp::Deref,
            _ => panic!("from_op: attempt to create UnaryOp from non-unary Op")
        }
    }
}

#[derive(Clone, Debug)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>
}

#[derive(Clone, Debug)]
pub enum Expr {
    ConstInt(i64),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
    If(IfExpr),
    Var(String),
    FunctionApp(String, Vec<Box<Expr>>),
    Block(Vec<Box<Expr>>)
}

#[derive(Clone, Debug)]
pub enum Definition {
    Function(String, Vec<String>, Box<Expr>)
}

#[derive(Clone, Debug)]
pub struct Module {
    pub definitions: Vec<Definition>
}

impl Module {
    pub fn new() -> Module {
        Module {
            definitions: Vec::new()
        }
    }
}