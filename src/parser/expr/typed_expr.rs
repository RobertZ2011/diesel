use super::{
    Expr,
    Type,
    ExprValue,
    IfExpr,
    BinOp
};

use crate::parser::token::Token;

use std::borrow::Borrow;

#[derive(Debug)]
pub struct TypedExpr<'a> {
    token: Token<'a>,
    /// Not actually a kind, but avoids having to call it r#type
    kind: Type,
    value: ExprValue<TypedExpr<'a>>
}

impl<'a> Expr for TypedExpr<'a> {}

impl<'a> TypedExpr<'a> {
    pub fn kind(&self) -> &Type {
        return &self.kind;
    }

    pub fn const_unit(token: Token<'a>) -> Box<TypedExpr<'a>> {
        Box::new(TypedExpr {
            token: token,
            kind: Type::Unit,
            value: ExprValue::ConstUnit
        })
    }

    pub fn const_int(token: Token<'a>, value: i64) -> Box<TypedExpr<'a>> {
        Box::new(TypedExpr {
            token: token,
            kind: Type::Int,
            value: ExprValue::ConstInt(value)
        })
    }

    pub fn const_bool(token: Token<'a>, value: bool) -> Box<TypedExpr<'a>> {
        Box::new(TypedExpr {
            token: token,
            kind: Type::Bool,
            value: ExprValue::ConstBool(value)
        })
    }

    pub fn bin_op(token: Token<'a>, kind: Type, op: BinOp, lhs: Box<TypedExpr<'a>>, rhs: Box<TypedExpr<'a>>) -> Box<TypedExpr<'a>> {
        Box::new(TypedExpr {
            token: token,
            kind: kind,
            value: ExprValue::BinOp(op, lhs, rhs)
        })
    }

    pub fn block(token: Token<'a>, exprs: Vec<Box<TypedExpr<'a>>>) -> Box<TypedExpr<'a>> {
        let block_type = if exprs.len() == 0 { Type::Unit } else { exprs[exprs.len() - 1].kind().clone() };
        Box::new(TypedExpr {
            token: token,
            kind: block_type,
            value: ExprValue::Block(exprs)
        })
    }

    pub fn conditional(token: Token<'a>, cond_expr: Box<TypedExpr<'a>>, then_expr: Box<TypedExpr<'a>>, else_expr: Box<TypedExpr<'a>>) -> Box<TypedExpr<'a>> {
        assert_eq!(*cond_expr.kind(), Type::Bool);
        assert_eq!(then_expr.kind(), else_expr.kind());

        Box::new(TypedExpr {
            token: token,
            kind: then_expr.kind().clone(),
            value: ExprValue::If(IfExpr {
                cond: cond_expr,
                then_expr: then_expr,
                else_expr: else_expr
            })
        })
    }

    pub fn var(token: Token<'a>, iden: &String, kind: Type) -> Box<TypedExpr<'a>> {
        Box::new(TypedExpr {
            token: token,
            kind: kind,
            value: ExprValue::Var(iden.clone())
        })
    }
}

impl<'a> Borrow<ExprValue<TypedExpr<'a>>> for TypedExpr<'a> {
    fn borrow(&self) -> &ExprValue<TypedExpr<'a>> {
        return &self.value;
    }
}