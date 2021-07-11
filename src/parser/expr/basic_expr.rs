use super::{
    Expr,
    ExprValue,
    UnaryOp,
    BinOp,
    IfExpr
};

use crate::parser::token::Token;

use std::borrow::Borrow;

#[derive(Clone, Debug)]
pub struct BasicExpr<'a> {
    token: Token<'a>,
    value: ExprValue<BasicExpr<'a>>
}

impl<'a> Expr for BasicExpr<'a> {}

impl<'a> BasicExpr<'a> {
    pub fn token(&self) -> &Token<'a> {
        return &self.token;
    }

    pub fn unary_op(token: Token<'a>, op: UnaryOp, expr: Box<BasicExpr<'a>>) -> Box<BasicExpr<'a>> {
        Box::new(BasicExpr {
            token: token,
            value: ExprValue::UnaryOp(op, expr)
        })
    }

    pub fn bin_op(token: Token<'a>, op: BinOp, lhs: Box<BasicExpr<'a>>, rhs: Box<BasicExpr<'a>>) -> Box<BasicExpr<'a>> {
        Box::new (BasicExpr {
            token: token,
            value: ExprValue::BinOp(op, lhs, rhs)
        })
    }

    pub fn function_app(token: Token<'a>, iden: String, args: Vec<Box<BasicExpr<'a>>>) -> Box<BasicExpr<'a>> {
        Box::new(BasicExpr {
            token: token,
            value: ExprValue::FunctionApp(iden, args)
        })
    }

    pub fn block(token: Token<'a>, exprs: Vec<Box<BasicExpr<'a>>>) -> Box<BasicExpr<'a>> {
        Box::new(BasicExpr {
            token: token,
            value: ExprValue::Block(exprs)
        })
    }

    pub fn conditional(token: Token<'a>, cond: Box<BasicExpr<'a>>, then_expr: Box<BasicExpr<'a>>, else_expr: Box<BasicExpr<'a>>) -> Box<BasicExpr<'a>> {
        Box::new(BasicExpr {
            token: token,
            value: ExprValue::If(IfExpr {
                cond: cond,
                then_expr: then_expr,
                else_expr: else_expr
            })
        })
    }

    pub fn const_int(token: Token<'a>, value: i64) -> Box<BasicExpr<'a>> {
        Box::new(BasicExpr {
            token: token,
            value: ExprValue::ConstInt(value)
        })
    }

    pub fn var(token: Token<'a>, iden: String) -> Box<BasicExpr<'a>> {
        Box::new(BasicExpr {
            token: token,
            value: ExprValue::Var(iden)
        })
    }
}

impl<'a> Borrow<ExprValue<BasicExpr<'a>>> for BasicExpr<'a> {
    fn borrow(&self) -> &ExprValue<BasicExpr<'a>> {
        return &self.value;
    }
}