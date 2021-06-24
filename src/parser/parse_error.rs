use crate::parser::{
    Token,
    TokenType
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