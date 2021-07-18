use crate::parser::{
    Token,
    TokenType,
    Op
};

#[derive(Clone, Debug)]
pub struct UnexpectedToken<'a> {
    found: Token<'a>,
    expected: Vec<TokenType>
}

impl<'a> UnexpectedToken<'a> {
    pub fn multiple(expected: &[TokenType], found: Token<'a>) -> UnexpectedToken<'a> {
        let mut ret = UnexpectedToken {
            found: found,
            expected: Vec::new()
        };

        ret.expected.extend_from_slice(expected);
        ret
    }

    pub fn single(expected: TokenType, found: Token) -> UnexpectedToken {
        UnexpectedToken {
            expected: vec![expected],
            found: found
        }
    }

    pub fn expected(&self) -> &[TokenType] {
        self.expected.as_slice()
    }

    pub fn found(&self) -> Token<'a> {
        self.found.clone()
    }
}

#[derive(Debug)]
pub struct OperandMismatch<'a> {
    pub token: Token<'a>,
    pub operator: Op,
    pub found: usize
}

#[derive(Debug)]
pub enum ParseError<'a> {
    UnexpectedToken(UnexpectedToken<'a>),
    OperandMismatch(OperandMismatch<'a>),

    MismatchedBrace(Token<'a>),
    MismatchedParen(Token<'a>)
}

impl<'a> ParseError<'a> {
    pub fn operand_mismatch<T>(token: Token<'a>, operator: Op, found: usize) -> Result<T, ParseError<'a>> {
        Err(ParseError::OperandMismatch(OperandMismatch{
            token: token,
            operator: operator,
            found: found
        }))
    }

    pub fn unexpected_token<T>(expected: TokenType, found: Token<'a>) -> Result<T, ParseError<'a>> {
        Err(ParseError::UnexpectedToken(UnexpectedToken::single(expected, found)))
    }

    pub fn unexpected_token_multi<T>(expected: &[TokenType], found: Token<'a>) -> Result<T, ParseError<'a>> {
        Err(ParseError::UnexpectedToken(UnexpectedToken::multiple(expected, found)))
    }
}