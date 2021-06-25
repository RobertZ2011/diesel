use std::iter::{
    Peekable,
    IntoIterator
};

use crate::parser::parse_error::UnexpectedToken;

use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    //Usual Arith
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    //Comparison
    Eq,
    Ne,
    Gte,
    Lte,
    Gt,
    Lt,

    //Unary
    Deref
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpAssociativity {
    Left,
    Right
}

impl Op {
    pub fn precedence(self) -> i8 {
        //Just following C for now, might be tweaks in the future
        match self {
            Op::Deref => 1,
            Op::Mul | Op::Div | Op::Mod => 2,
            Op::Add | Op::Sub => 3,
            Op::Gt | Op::Lt | Op::Gte | Op::Lte => 4,
            Op::Eq | Op::Ne => 5
        }
    }

    pub fn associativity(self) -> OpAssociativity {
        //Don't have any right assoc operators yet
        OpAssociativity::Left
    }

    pub fn is_unary(self) -> bool {
        match self {
            Op::Deref => true,
            _ => false
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenValue<'a> {
    Operator(Op),
    Identifier(&'a str),
    ConstInt(i64),

    //Key words
    Function,
    If,
    Else,

    //Symbols
    Comma,
    Semicolon,
    LParen, //(
    RParen, //)
    LCBracket, //{
    RCBracket, //}

    //Misc
    Comment,
    Eof
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    Identifier,
    Operator,
    Int,

    //Key words
    Function,
    If,
    Else,

    //Symbols
    Comma,
    Semicolon,
    LParen, //(
    RParen, //)
    LBrace, //{
    RBrace, //}

    //Misc
    Comment,
    Eof
}

impl<'a> TokenValue<'a> {
    pub fn token_type(&self) -> TokenType {
        match self {
            TokenValue::Operator(_) => TokenType::Operator,
            TokenValue::Identifier(_) => TokenType::Identifier,
            TokenValue::ConstInt(_) => TokenType::Int,

            TokenValue::Function => TokenType::Function,
            TokenValue::If => TokenType::If,
            TokenValue::Else => TokenType::Else,

            TokenValue::Comma => TokenType::Comma,
            TokenValue::Semicolon => TokenType::Semicolon,
            TokenValue::LParen => TokenType::LParen,
            TokenValue::RParen => TokenType::RParen,
            TokenValue::LCBracket => TokenType::LBrace,
            TokenValue::RCBracket => TokenType::RBrace,

            TokenValue::Comment => TokenType::Comment,
            TokenValue::Eof => TokenType::Eof
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub pos: Span<'a>,
    pub value: TokenValue<'a>
}

impl<'a> Token<'a> {
    pub fn operator(op: Op, pos: Span) -> Token {
        Token {
            pos: pos,
            value: TokenValue::Operator(op)
        }
    }

    pub fn identifier(iden: &'a str, pos: Span<'a>) -> Token<'a> {
        Token {
            pos: pos,
            value: TokenValue::Identifier(iden)
        }
    }

    pub fn const_int(value: i64, pos: Span) -> Token {
        Token {
            pos: pos,
            value: TokenValue::ConstInt(value)
        }
    }

    pub fn eof() -> Token<'static> {
        Token {
            pos: Span::new(""),
            value: TokenValue::Eof
        }
    }
}

pub type TokenResult<'a, T> = Result<T, UnexpectedToken<'a>>;

#[derive(Debug)]
pub struct TokenStream<'a, T: IntoIterator<Item = Token<'a>>> {
    iter: Peekable<T::IntoIter>
}

impl<'a, T: IntoIterator<Item = Token<'a>>> TokenStream<'a, T> {
    pub fn new(tokens: T) -> TokenStream<'a, T> {
        TokenStream {
            iter: tokens.into_iter().peekable()
        }
    }

    pub fn peek_token_type(&mut self) -> Option<TokenType> {
        while let Some(token_type) = self.iter.peek().map(|token| token.value.token_type()) {
            if token_type == TokenType::Comment {
                let _ = self.consume();
            }
            else {
                return Some(token_type);
            }
        }

        None
    }

    pub fn consume(&mut self) -> Option<Token<'a>> {
        self.iter.next()
    }

    pub fn skip_comments(&mut self) {
        while let Some(token_type) = self.peek_token_type() {
            if token_type == TokenType::Comment {
                let _ = self.consume();
            }
            else {
                break;
            }
        }
    }

    pub fn expect_op(&mut self) -> TokenResult<'a, Op> {
        self.skip_comments();

        if let Some(token) = self.iter.next() {
            if let Token { value: TokenValue::Operator(op), .. } = token {
                Ok(op)
            }
            else {
                Err(UnexpectedToken::single(TokenType::Operator, token))
            }
        }
        else {
            Err(UnexpectedToken::single(TokenType::Operator, Token::eof()))
        }
    }

    pub fn consume_op(&mut self) -> Op {
        if let Some(Token { value: TokenValue::Operator(op), .. }) = self.iter.next() {
            op
        }
        else {
            panic!("consume_op: next token is not an operator")
        }
    }

    pub fn expect_identifier(&mut self) -> TokenResult<'a, &'a str> {
        self.skip_comments();

        if let Some(token) = self.iter.next() {
            if let Token { value: TokenValue::Identifier(iden), .. } = token {
                Ok(iden)
            }
            else {
                Err(UnexpectedToken::single(TokenType::Identifier, token))
            }
        }
        else {
            Err(UnexpectedToken::single(TokenType::Identifier, Token::eof()))
        }
    }

    pub fn consume_identifier(&mut self) -> &'a str {
        if let Some(Token { value: TokenValue::Identifier(iden), .. }) = self.iter.next() {
            iden
        }
        else {
            panic!("consume_identifer: next token is not an identifier");
        }
    }

    pub fn expect_int(&mut self) -> TokenResult<'a, i64> {
        self.skip_comments();

        if let Some(token) = self.iter.next() {
            if let Token { value: TokenValue::ConstInt(value), .. } = token {
                Ok(value)
            }
            else {
                Err(UnexpectedToken::single(TokenType::Int, token))
            }
        }
        else {
            Err(UnexpectedToken::single(TokenType::Int, Token::eof()))
        }
    }

    pub fn consume_int(&mut self) -> i64 {
        if let Some(Token { value: TokenValue::ConstInt(value), .. }) = self.iter.next() {
            value
        }
        else {
            panic!("consume_int: next token is not an int")
        }
    }

    pub fn expect_function(&mut self) -> TokenResult<'a, ()> {
        self.skip_comments();

        if let Some(token) = self.iter.next() {
            match token.value {
                TokenValue::Function => Ok(()),
                _ => Err(UnexpectedToken::single(TokenType::Function, token))
            }
        }
        else {
            Err(UnexpectedToken::single(TokenType::Function, Token::eof()))
        }
    }

    pub fn expect_lparen(&mut self) -> TokenResult<'a, ()> {
        self.skip_comments();

        if let Some(token) = self.iter.next() {
            match token.value {
                TokenValue::LParen => Ok(()),
                _ => Err(UnexpectedToken::single(TokenType::LParen, token))
            }
        }
        else {
            Err(UnexpectedToken::single(TokenType::Function, Token::eof()))
        }
    }
}