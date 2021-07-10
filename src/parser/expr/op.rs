use crate::parser::token::Op;

use std::convert::From;

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

impl From<Op> for BinOp {
    fn from(op: Op) -> BinOp {
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

impl From<Op> for UnaryOp {
    fn from(op: Op) -> UnaryOp {
        match op {
            Op::Deref => UnaryOp::Deref,
            _ => panic!("from_op: attempt to create UnaryOp from non-unary Op")
        }
    }
}