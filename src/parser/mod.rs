use nom::{
    IResult,
    character::complete::{alpha1, alphanumeric0},
    multi::{many0}
};

mod expr;
mod top_level;

use crate::ast::TopLevel;

#[derive(Debug)]
pub struct Module {
    pub top_level: Vec<TopLevel>
}

fn identifier(i: &str) -> IResult<&str, String> {
    let (i, initial) = alpha1(i)?;
    let (i, rest) = alphanumeric0(i)?;
    
    let mut res = String::from(initial);
    res.push_str(rest);
    Ok((i, res))
}

pub fn module(i: &str) -> IResult<&str, Module> {
    let (i, top) = many0(top_level::top_level)(i)?;
    Ok((i, Module {
        top_level: top
    }))
}