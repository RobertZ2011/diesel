use std::iter::{
    Peekable,
    IntoIterator
};

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
pub enum Token<'a> {
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
    Comment
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
    LCBracket, //{
    RCBracket, //}

    //Misc
    Comment,
    Eof
}

impl<'a> Token<'a> {
    pub fn token_type(&self) -> TokenType {
        match self {
            Token::Operator(_) => TokenType::Operator,
            Token::Identifier(_) => TokenType::Identifier,
            Token::ConstInt(_) => TokenType::Int,

            Token::Function => TokenType::Function,
            Token::If => TokenType::If,
            Token::Else => TokenType::Else,

            Token::Comma => TokenType::Comma,
            Token::Semicolon => TokenType::Semicolon,
            Token::LParen => TokenType::LParen,
            Token::RParen => TokenType::RParen,
            Token::LCBracket => TokenType::LCBracket,
            Token::RCBracket => TokenType::RCBracket,

            Token::Comment => TokenType::Comment
        }
    }
}

#[derive(Clone, Debug)]
pub struct UnexpectedToken {
    found: TokenType,
    expected: Vec<TokenType>
}

impl UnexpectedToken {
    pub fn multiple(expected: &[TokenType], found: TokenType) -> UnexpectedToken {
        let mut ret = UnexpectedToken {
            found: found,
            expected: Vec::new()
        };

        ret.expected.extend_from_slice(expected);
        ret
    }

    pub fn single(expected: TokenType, found: TokenType) -> UnexpectedToken {
        UnexpectedToken {
            expected: vec![expected],
            found: found
        }
    }

    pub fn expected<'a>(&'a self) -> &'a [TokenType] {
        self.expected.as_slice()
    }

    pub fn found(&self) ->TokenType {
        self.found
    }
}

pub type TokenResult<T> = Result<T, UnexpectedToken>;

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
        while let Some(token_type) = self.iter.peek().map(|token| token.token_type()) {
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

    pub fn expect_op(&mut self) -> TokenResult<Op> {
        self.skip_comments();

        if let Some(token) = self.iter.next() {
            if let Token::Operator(op) = token {
                Ok(op)
            }
            else {
                Err(UnexpectedToken::single(TokenType::Operator, token.token_type()))
            }
        }
        else {
            Err(UnexpectedToken::single(TokenType::Operator, TokenType::Eof))
        }
    }

    pub fn consume_op(&mut self) -> Op {
        if let Some(Token::Operator(op)) = self.iter.next() {
            op
        }
        else {
            panic!("consume_op: next token is not an operator")
        }
    }

    pub fn expect_identifier(&mut self) -> TokenResult<&'a str> {
        self.skip_comments();

        if let Some(token) = self.iter.next() {
            if let Token::Identifier(iden) = token {
                Ok(iden)
            }
            else {
                Err(UnexpectedToken::single(TokenType::Identifier, token.token_type()))
            }
        }
        else {
            Err(UnexpectedToken::single(TokenType::Identifier, TokenType::Eof))
        }
    }

    pub fn consume_identifier(&mut self) -> &'a str {
        if let Some(Token::Identifier(iden)) = self.iter.next() {
            iden
        }
        else {
            panic!("consume_identifer: next token is not an identifier");
        }
    }

    pub fn expect_int(&mut self) -> TokenResult<i64> {
        self.skip_comments();

        if let Some(token) = self.iter.next() {
            if let Token::ConstInt(value) = token {
                Ok(value)
            }
            else {
                Err(UnexpectedToken::single(TokenType::Int, token.token_type()))
            }
        }
        else {
            Err(UnexpectedToken::single(TokenType::Int, TokenType::Eof))
        }
    }

    pub fn consume_int(&mut self) -> i64 {
        if let Some(Token::ConstInt(value)) = self.iter.next() {
            value
        }
        else {
            panic!("consume_int: next token is not an int")
        }
    }

    pub fn expect_function(&mut self) -> TokenResult<()> {
        self.skip_comments();

        if let Some(token) = self.iter.next() {
            match token {
                Token::Function => Ok(()),
                _ => Err(UnexpectedToken::single(TokenType::Function, token.token_type()))
            }
        }
        else {
            Err(UnexpectedToken::single(TokenType::Function, TokenType::Eof))
        }
    }

    pub fn expect_lparen(&mut self) -> TokenResult<()> {
        self.skip_comments();

        if let Some(token) = self.iter.next() {
            match token {
                Token::LParen => Ok(()),
                _ => Err(UnexpectedToken::single(TokenType::LParen, token.token_type()))
            }
        }
        else {
            Err(UnexpectedToken::single(TokenType::Function, TokenType::Eof))
        }
    }
}