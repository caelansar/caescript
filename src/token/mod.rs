use std::{fmt, str::FromStr};

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Token {
    Illegal,
    EOF,

    Ident(String),
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),

    Assign,
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Asterisk,
    AsteriskEq,
    Slash,
    SlashEq,
    Comma,
    SemiColon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,
    Gt,
    Lt,
    GtEq,
    LtEq,
    Bang,
    Eq,
    Ne,
    Colon,
    Function,
    Let,
    If,
    Else,
    For,
    Return,
    Comment,
}

impl FromStr for Token {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "let" => Ok(Token::Let),
            "true" => Ok(Token::Bool(true)),
            "false" => Ok(Token::Bool(false)),
            "fn" => Ok(Token::Function),
            "if" => Ok(Token::If),
            "else" => Ok(Token::Else),
            "for" => Ok(Token::For),
            "return" => Ok(Token::Return),
            "=" => Ok(Token::Assign),
            "+" => Ok(Token::Plus),
            "-" => Ok(Token::Minus),
            "!" => Ok(Token::Bang),
            "*" => Ok(Token::Asterisk),
            "/" => Ok(Token::Slash),
            "<" => Ok(Token::Lt),
            "<=" => Ok(Token::LtEq),
            ">" => Ok(Token::Gt),
            ">=" => Ok(Token::GtEq),
            "==" => Ok(Token::Eq),
            "!=" => Ok(Token::Ne),
            "," => Ok(Token::Comma),
            ";" => Ok(Token::SemiColon),
            ":" => Ok(Token::Colon),
            "(" => Ok(Token::Lparen),
            ")" => Ok(Token::Rparen),
            "{" => Ok(Token::Lbrace),
            "}" => Ok(Token::Rbrace),
            "[" => Ok(Token::Lbracket),
            "]" => Ok(Token::Rbracket),
            _ => Err("unknow token str"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(name) => write!(f, "{}", name),
            Token::Int(i) => write!(f, "{}", i),
            Token::Float(i) => write!(f, "{}", i),
            Token::String(s) => write!(f, "{}", s),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::PlusEq => write!(f, "+="),
            Token::MinusEq => write!(f, "-="),
            Token::AsteriskEq => write!(f, "*="),
            Token::SlashEq => write!(f, "/="),
            Token::Bang => write!(f, "!"),
            Token::Lt => write!(f, "<"),
            Token::LtEq => write!(f, "<="),
            Token::Gt => write!(f, ">"),
            Token::GtEq => write!(f, ">="),
            Token::Eq => write!(f, "=="),
            Token::Ne => write!(f, "!="),
            Token::Comma => write!(f, ","),
            Token::SemiColon => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Lbracket => write!(f, "["),
            Token::Rbracket => write!(f, "]"),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::For => write!(f, "for"),
            Token::Return => write!(f, "return"),
            Token::Illegal => write!(f, "ILLEGAL"),
            Token::Comment => write!(f, "//comment"),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

pub(crate) fn lookup_ident(key: impl AsRef<str>) -> Token {
    match key.as_ref().parse() {
        Ok(tok) => tok,
        Err(_) => Token::Ident(key.as_ref().to_string()),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn token_should_work() {
        assert_eq!(String::from("="), Token::Assign.to_string());
        assert_eq!(String::from(","), Token::Comma.to_string());
        assert_eq!(String::from("fn"), Token::Function.to_string());
        assert_eq!(Token::Rbrace, Token::from_str("}").unwrap());
        assert_eq!(Token::Let, lookup_ident("let"));
        assert_eq!(Token::Function, lookup_ident("fn"));
        assert_eq!(Token::Ident("func".to_string()), lookup_ident("func"));
        assert_eq!(Token::Else, lookup_ident("else"));
        assert_eq!(Token::For, lookup_ident("for"));
    }
}
