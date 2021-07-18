use super::Expr;
use super::{
    BinOp,
    UnaryOp
};

use std::fmt::{
    Debug,
    Formatter
};

#[derive(Clone, Debug)]
pub struct IfExpr<E: Expr> {
    pub cond: Box<E>,
    pub then_expr: Box<E>,
    pub else_expr: Box<E>
}

/// Enum to represent all possibile expressions
/// Generic to support different expression types at different stages
#[derive(Clone, Debug)]
pub enum ExprValue<E: Expr> {
    //Constant values
    ConstUnit,
    ConstInt(i64),
    ConstBool(bool),
    ConstDouble(f64),

    //Operators
    BinOp(BinOp, Box<E>, Box<E>),
    UnaryOp(UnaryOp, Box<E>),

    //Misc
    If(IfExpr<E>),
    Var(String),
    FunctionApp(String, Vec<Box<E>>),
    Block(Vec<Box<E>>)
}