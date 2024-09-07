use std::{fmt, mem, str::FromStr};

#[derive(Debug, Clone)]
pub enum TokenError {
    UnknowToken(char),
    UnterminatedString,
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknowToken(token) => write!(f, "Unknown token: '{}'", token),
            Self::UnterminatedString => write!(f, "Unterminated string"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    Illegal(TokenError),
    Eof,

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
    Mod,
    ModEq,
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
    Break,
    Continue,
    Return,
    And,
    Or,
    LeftShift,
    LeftShiftEq,
    RightShift,
    RightShiftEq,
    BitAnd,
    BitAndEq,
    BitOr,
    BitOrEq,
    BitXor,
    BitXorEq,
    Null,
}

impl Eq for Token {}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        mem::discriminant(self).eq(&mem::discriminant(other))
    }
}

impl std::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state)
    }
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
            "break" => Ok(Token::Break),
            "continue" => Ok(Token::Continue),
            "return" => Ok(Token::Return),
            "and" => Ok(Token::And),
            "or" => Ok(Token::Or),
            "null" => Ok(Token::Null),
            "=" => Ok(Token::Assign),
            "+" => Ok(Token::Plus),
            "-" => Ok(Token::Minus),
            "!" => Ok(Token::Bang),
            "*" => Ok(Token::Asterisk),
            "/" => Ok(Token::Slash),
            "%" => Ok(Token::Mod),
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
            "&" => Ok(Token::BitAnd),
            "|" => Ok(Token::BitOr),
            "^" => Ok(Token::BitXor),
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
            Token::Mod => write!(f, "%"),
            Token::ModEq => write!(f, "%="),
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
            Token::Illegal(e) => write!(f, "{e}"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Null => write!(f, "null"),
            Token::LeftShift => write!(f, "<<"),
            Token::LeftShiftEq => write!(f, "<<="),
            Token::RightShift => write!(f, ">>"),
            Token::RightShiftEq => write!(f, ">>="),
            Token::BitAnd => write!(f, "&"),
            Token::BitAndEq => write!(f, "&="),
            Token::BitOr => write!(f, "|"),
            Token::BitOrEq => write!(f, "|="),
            Token::BitXor => write!(f, "^"),
            Token::BitXorEq => write!(f, "^="),
            Token::Eof => write!(f, "EOF"),
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
    use std::{collections::HashMap, str::FromStr};

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
        assert_eq!(Token::Null, lookup_ident("null"));
    }

    #[test]
    fn test_token_hash() {
        let mut map = HashMap::new();
        map.insert(Token::String("".into()), 1);

        let v = map.get(&Token::String("1".into()));
        assert_eq!(1, *v.unwrap());
    }
}
