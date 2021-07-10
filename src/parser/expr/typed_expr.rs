use super::{
    Expr,
    Type,
    ExprValue
};

use crate::parser::token::Token;

pub struct TypedExpr<'a> {
    token: Token<'a>,
    /// Not actually a kind, but avoids having to call it r#type
    kind: Type,
    value: ExprValue<TypedExpr<'a>>
}

impl<'a> Expr for TypedExpr<'a> {

}