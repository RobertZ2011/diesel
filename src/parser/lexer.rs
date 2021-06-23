
use nom::{
    IResult,
    bytes::complete::tag,
    branch::alt,
    character::complete::{digit1, multispace0, alpha1},
    multi::many0
};

use crate::parser::token::{
    Token,
    Op,
    TokenStream
};

fn identifier<'a>(i: &'a str) -> IResult<&'a str, Token<'a>> {
    let(i, iden) = alpha1(i)?;
    Ok((i, Token::Identifier(iden)))
}

fn const_int<'a>(i: &'a str) -> IResult<&'a str, Token<'a>> {
    let (i, digits) = digit1(i)?;
    Ok((i, Token::ConstInt(digits.parse::<i64>().unwrap())))
}

fn operator<'a>(i: &'a str) -> IResult<&'a str, Token<'a>> {
    let (i, op) = alt((
        |i| tag("+")(i).map(|(i, _)| (i, Op::Add)),
        |i| tag("-")(i).map(|(i, _)| (i, Op::Sub)),
        |i| tag("*")(i).map(|(i, _)| (i, Op::Mul)),
        |i| tag("/")(i).map(|(i, _)| (i, Op::Div)),
        |i| tag("%")(i).map(|(i, _)| (i, Op::Mod)),

        |i| tag("==")(i).map(|(i, _)| (i, Op::Eq)),
        |i| tag("!=")(i).map(|(i, _)| (i, Op::Ne)),
        |i| tag(">=")(i).map(|(i, _)| (i, Op::Gte)),
        |i| tag("<=")(i).map(|(i, _)| (i, Op::Lte)),
        |i| tag(">")(i).map(|(i, _)| (i, Op::Gt)),
        |i| tag("<")(i).map(|(i, _)| (i, Op::Lt)),

        |i| tag("@")(i).map(|(i, _)| (i, Op::Deref))
    ))(i)?;

    Ok((i, Token::Operator(op)))
}

fn keyword<'a>(i: &'a str) -> IResult<&'a str, Token<'a>> {
    alt((
        |i| tag("function")(i).map(|(i, _)| (i, Token::Function)),
        |i| tag("if")(i).map(|(i, _)| (i, Token::If)),
        |i| tag("else")(i).map(|(i, _)| (i, Token::Else))
    ))(i)
}

fn symbol<'a>(i: &'a str) -> IResult<&'a str, Token<'a>> {
    alt((
        |i| tag(",")(i).map(|(i, _)| (i, Token::Comma)),
        |i| tag(";")(i).map(|(i, _)| (i, Token::Semicolon)),
        |i| tag("(")(i).map(|(i, _)| (i, Token::LParen)),
        |i| tag(")")(i).map(|(i, _)| (i, Token::RParen)),
        |i| tag("{")(i).map(|(i, _)| (i, Token::LCBracket)),
        |i| tag("}")(i).map(|(i, _)| (i, Token::RCBracket))
    ))(i)
}

fn token<'a>(i: &'a str) -> IResult<&'a str, Token<'a>> {
    let (i, _) = multispace0(i)?;
    alt((
        keyword,
        identifier,
        const_int,
        operator,
        symbol
    ))(i)
}

pub fn tokens<'a>(i: &'a str) -> IResult<&'a str, TokenStream<'a, Vec<Token<'a>>>> {
    let (i, tokens) = many0(token)(i)?;
    Ok((i, TokenStream::new(tokens)))
}