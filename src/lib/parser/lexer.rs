
use nom::{
    IResult,
    bytes::complete::tag,
    branch::alt,
    character::complete::{digit1, multispace0, alpha1},
    multi::many0
};

use nom_locate::position;

use crate::parser::token::{
    Token,
    TokenValue,
    Op,
    TokenStream,
    Span
};

fn identifier(s: Span) -> IResult<Span, Token> {
    let (s, iden) = alpha1(s)?;
    let (s, pos) = position(s)?;
    let token = Token::identifier(&iden, pos);
    Ok((s, token))
}

fn const_int(s: Span) -> IResult<Span, Token> {
    let (s, digits) = digit1(s)?;
    let (s, pos) = position(s)?;
    let token = Token::const_int(digits.parse::<i64>().unwrap(), pos);
    Ok((s, token))
}

fn const_unit(s: Span) -> IResult<Span, Token> {
    let (s, _) = tag("Unit")(s)?;
    let (s, pos) = position(s)?;
    let token = Token {
        pos: pos,
        value: TokenValue::Unit
    };
    Ok((s, token))
}

fn const_bool(s: Span) -> IResult<Span, Token> {
    let (s, value) = alt((
        |s| tag("true")(s).map(|(s, _)| (s, TokenValue::ConstBool(true))),
        |s| tag("false")(s).map(|(s, _)| (s, TokenValue::ConstBool(false)))
    ))(s)?;
    let (s, pos) = position(s)?;
    let token = Token {
        pos: pos,
        value: value
    };
    Ok((s, token))
}

fn constant(s: Span) -> IResult<Span, Token> {
    alt((
        const_int,
        const_unit,
        const_bool
    ))(s)
}

fn operator(s: Span) -> IResult<Span, Token> {
    let (s, op) = alt((
        |s| tag("+")(s).map(|(s, _)| (s, Op::Add)),
        |s| tag("-")(s).map(|(s, _)| (s, Op::Sub)),
        |s| tag("*")(s).map(|(s, _)| (s, Op::Mul)),
        |s| tag("/")(s).map(|(s, _)| (s, Op::Div)),
        |s| tag("%")(s).map(|(s, _)| (s, Op::Mod)),

        |s| tag("==")(s).map(|(s, _)| (s, Op::Eq)),
        |s| tag("!=")(s).map(|(s, _)| (s, Op::Ne)),
        |s| tag(">=")(s).map(|(s, _)| (s, Op::Gte)),
        |s| tag("<=")(s).map(|(s, _)| (s, Op::Lte)),
        |s| tag(">")(s).map(|(s, _)| (s, Op::Gt)),
        |s| tag("<")(s).map(|(s, _)| (s, Op::Lt)),

        |s| tag("@")(s).map(|(s, _)| (s, Op::Deref))
    ))(s)?;
    let (s, pos) = position(s)?;

    Ok((s, Token::operator(op, pos)))
}

fn keyword(s: Span) -> IResult<Span, Token> {
    let (s, value) = alt((
        |s| tag("function")(s).map(|(s, _)| (s, TokenValue::Function)),
        |s| tag("if")(s).map(|(s, _)| (s, TokenValue::If)),
        |s| tag("else")(s).map(|(s, _)| (s, TokenValue::Else))
    ))(s)?;

    let (s, pos) = position(s)?;
    let token = Token {
        pos: pos,
        value: value
    };
    Ok((s, token))
}

fn kind(s: Span) -> IResult<Span, Token> {
    let (s, kind) = alt((
        |s| tag("Unit")(s).map(|(s, _)| (s, TokenValue::Unit)),
        |s| tag("Int")(s).map(|(s, _)| (s, TokenValue::Int)),
        |s| tag("Bool")(s).map(|(s, _)| (s, TokenValue::Bool)),
        |s| tag("Double")(s).map(|(s, _)| (s, TokenValue::Double))
    ))(s)?;

    let (s, pos) = position(s)?;
    let token = Token {
        pos: pos,
        value: kind
    };
    Ok((s, token))
}

fn symbol(s: Span) -> IResult<Span, Token> {
    let (s, value) = alt((
        |s| tag(",")(s).map(|(s, _)| (s, TokenValue::Comma)),
        |s| tag(";")(s).map(|(s, _)| (s, TokenValue::Semicolon)),
        |s| tag("(")(s).map(|(s, _)| (s, TokenValue::LParen)),
        |s| tag(")")(s).map(|(s, _)| (s, TokenValue::RParen)),
        |s| tag("{")(s).map(|(s, _)| (s, TokenValue::LCBracket)),
        |s| tag("}")(s).map(|(s, _)| (s, TokenValue::RCBracket)),
        |s| tag(":")(s).map(|(s, _)| (s, TokenValue::Colon)),
        |s| tag("->")(s).map(|(s, _)| (s, TokenValue::RArrow))
    ))(s)?;

    let (s, pos) = position(s)?;
    let token = Token {
        pos: pos,
        value: value
    };

    Ok((s, token))
}

fn token(s: Span) -> IResult<Span, Token> {
    let (s, _) = multispace0(s)?;
    alt((
        keyword,
        kind,
        identifier,
        constant,
        symbol,
        operator
    ))(s)
}

pub fn tokens(s: Span) -> IResult<Span, TokenStream<Vec<Token>>> {
    let (s, tokens) = many0(token)(s)?;
    Ok((s, TokenStream::new(tokens)))
}