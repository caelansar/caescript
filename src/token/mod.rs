use strum_macros::Display;
use strum_macros::EnumString;

#[derive(EnumString, Display, Debug, PartialEq, Clone, Copy)]
pub(crate) enum TokenType {
    Illegal,
    EOF,
    Ident,
    Int,
    #[strum(serialize = "=")]
    Assign,
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = ",")]
    Comma,
    #[strum(serialize = ";")]
    SimiColon,
    #[strum(serialize = "(")]
    Lparen,
    #[strum(serialize = ")")]
    Rparen,
    #[strum(serialize = "{")]
    Lbrace,
    #[strum(serialize = "}")]
    Rbrace,
    Function,
    Let,
}

#[derive(Debug)]
pub(crate) struct Token {
    pub typ: TokenType,
    pub literal: String,
}

impl Token {
    pub(crate) fn new(typ: TokenType, literal: String) -> Self {
        Self { typ, literal }
    }

    pub(crate) fn lookup_ident(key: impl AsRef<str>) -> TokenType {
        KEY_WORDS
            .binary_search_by(|(k, _)| k.cmp(&key.as_ref()))
            .map(|x| KEY_WORDS[x].1)
            .ok()
            .or(Some(TokenType::Ident))
            .unwrap()
    }
}

static KEY_WORDS: &[(&'static str, TokenType)] =
    &[("fn", TokenType::Function), ("let", TokenType::Let)];

#[cfg(test)]
mod test {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn token_should_work() {
        assert_eq!(String::from("="), TokenType::Assign.to_string());
        assert_eq!(String::from(","), TokenType::Comma.to_string());
        assert_eq!(String::from("Function"), TokenType::Function.to_string());
        assert_eq!(TokenType::Rbrace, TokenType::from_str("}").unwrap());
        assert_eq!(TokenType::Let, Token::lookup_ident("let"));
        assert_eq!(TokenType::Function, Token::lookup_ident("fn"));
        assert_eq!(TokenType::Ident, Token::lookup_ident("func"));
    }
}
