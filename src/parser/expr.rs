use nom::{
    IResult,
    character::complete::{digit1, multispace0, char, alpha1, alphanumeric0},
    branch::{alt},
    multi::{separated_list0}
};

use crate::ast::{
    BinOp,
    Expr
};

use super::identifier;

fn const_int(i: &str) -> IResult<&str, Box<Expr>> {
    let (i, digits) = digit1(i)?;
    let int = digits.parse::<i64>().unwrap();
    Ok((i, Box::new(Expr::ConstInt(int))))
}

fn paren(i: &str) -> IResult<&str, Box<Expr>> {
    let (i, _) = char('(')(i)?;
    let (i, expr) = expr(i)?;
    let (i, _) = char(')')(i)?;
    Ok((i, expr))
}

fn function_app(i: &str) -> IResult<&str, Box<Expr>> {
    let (i, name) = identifier(i)?;
    let (i, _) = char('(')(i)?;
    let (i, args) = separated_list0(char(','), expr)(i)?;
    let (i, _) = char(')')(i)?;
    Ok((i, Box::new(Expr::FunctionApp(name, args))))
}

fn var(i: &str) -> IResult<&str, Box<Expr>> {
    let (i, name) = identifier(i)?;
    Ok((i, Box::new(Expr::Var(name))))
}

fn block(i: &str) -> IResult<&str, Box<Expr>> {
    let (i, _) = char('{')(i)?;
    let (i, exprs) = separated_list0(char(';'), expr)(i)?;
    let (i, _) = char('}')(i)?;
    Ok((i, Box::new(Expr::Block(exprs))))
}

fn simple_expr(i: &str) -> IResult<&str, Box<Expr>> {
    alt((const_int, block, paren, function_app, var))(i)
}

fn mul(i: &str) -> IResult<&str, Box<Expr>> {
    let (i, lhs) = simple_expr(i)?;
    let (i, _) = char('*')(i)?;
    let (i, rhs) = mul_div(i)?;
    Ok((i, Box::new(Expr::BinOp(BinOp::Mul, lhs, rhs))))
}

fn div(i: &str) -> IResult<&str, Box<Expr>> {
    let (i, lhs) = simple_expr(i)?;
    let (i, _) = char('/')(i)?;
    let (i, rhs) = mul_div(i)?;
    Ok((i, Box::new(Expr::BinOp(BinOp::Div, lhs, rhs))))
}

fn mul_div(i: &str) -> IResult<&str, Box<Expr>> {
    alt((mul, div, simple_expr))(i)
}

fn add(i: &str) -> IResult<&str, Box<Expr>> {
    let (i, lhs) = mul_div(i)?;
    let (i, _) = char('+')(i)?;
    let (i, rhs) = add_sub(i)?;
    Ok((i, Box::new(Expr::BinOp(BinOp::Add, lhs, rhs))))
}

fn sub(i: &str) -> IResult<&str, Box<Expr>> {
    let (i, lhs) = mul_div(i)?;
    let (i, _) = char('-')(i)?;
    let (i, rhs) = add_sub(i)?;
    Ok((i, Box::new(Expr::BinOp(BinOp::Sub, lhs, rhs))))
}

fn add_sub(i: &str) -> IResult<&str, Box<Expr>> {
    alt((add, sub, mul_div))(i)
}

pub fn expr(i: &str) -> IResult<&str, Box<Expr>> {
    let (i, _) = multispace0(i)?;
    add_sub(i)
}