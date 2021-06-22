use nom:: {
    IResult,
    character::complete::{multispace1, multispace0, char},
    bytes::complete::{tag},
    multi::{separated_list0}
};

use crate::ast::{
    TopLevel,
    Expr
};

use crate::parser::{
    expr::expr,
    identifier
};

fn function_def(i: &str) -> IResult<&str, TopLevel> {
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("function")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = identifier(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = char('(')(i)?;
    let (i, vars) = separated_list0(char(','), identifier)(i)?;
    let (i, _) = char(')')(i)?;
    let (i, e) = expr(i)?;
    Ok((i, TopLevel::FunctionDef(name, vars, e)))
}

pub fn top_level(i: &str) -> IResult<&str, TopLevel> {
    function_def(i)
}