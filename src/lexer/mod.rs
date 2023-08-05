use crate::token::{lookup_ident, Token};
use std::str::FromStr;

#[derive(Default)]
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    next_pos: usize,
    ch: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            ..Default::default()
        };
        l.read_char();
        l
    }

    fn peek_char(&self) -> Option<char> {
        if self.next_pos >= self.input.len() {
            None
        } else {
            self.input.as_bytes().get(self.next_pos).map(|x| *x as char)
        }
    }

    fn read_char(&mut self) {
        self.skip_comment();
        if self.next_pos >= self.input.len() {
            self.ch = None
        } else {
            self.ch = Some(self.input.as_bytes()[self.next_pos] as char);
        }
        self.pos = self.next_pos;
        self.next_pos += 1;
    }

    fn skip_comment(&mut self) {
        if self.ch == Some('/') && self.peek_char() == Some('/') {
            if let Some(pos) = self.input[self.pos..].find('\n') {
                self.ch = Some(self.input.as_bytes()[self.pos + pos] as char);
                self.pos += pos - 1;
                self.next_pos = self.pos + 1;
            } else {
                // the comment is in last line
                self.ch = None;
                self.pos = self.input.len();
                self.next_pos = self.pos + 1;
            }
        }
    }

    fn is_identifier(ch: char) -> bool {
        ch.is_alphanumeric() || ch == '_'
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.pos;
        while self.ch.map(Self::is_identifier).is_some_and(|t| t) {
            self.read_char();
        }
        self.input[pos..self.pos].to_string()
    }

    fn read_number(&mut self) -> String {
        let pos = self.pos;
        while self.ch.map(|c| c.is_numeric()).is_some_and(|t| t) {
            self.read_char();
        }
        self.input[pos..self.pos].to_string()
    }

    fn read_string(&mut self) -> String {
        let pos = self.pos + 1;
        loop {
            self.read_char();
            if self.ch.is_none() || self.ch.is_some_and(|x| x == '"') {
                break;
            }
        }
        self.input[pos..self.pos].to_string()
    }

    #[inline(always)]
    fn eat_whitespace(&mut self) {
        while self.ch.map(|c| c.is_whitespace()).is_some_and(|x| x) {
            self.read_char()
        }
    }

    pub(crate) fn next_token(&mut self) -> Token {
        self.eat_whitespace();

        let tok = if let Some(token) = self.ch {
            match token {
                '=' => {
                    if let Some('=') = self.peek_char() {
                        self.read_char();
                        Token::Eq
                    } else {
                        Token::from_str(token.to_string().as_str()).unwrap()
                    }
                }
                '!' => {
                    if let Some('=') = self.peek_char() {
                        self.read_char();
                        Token::Ne
                    } else {
                        Token::from_str(token.to_string().as_str()).unwrap()
                    }
                }
                '>' => {
                    if let Some('=') = self.peek_char() {
                        self.read_char();
                        Token::GtEq
                    } else {
                        Token::Gt
                    }
                }
                '<' => {
                    if let Some('=') = self.peek_char() {
                        self.read_char();
                        Token::LtEq
                    } else {
                        Token::Lt
                    }
                }
                '"' => Token::String(self.read_string()),
                '+' => {
                    if let Some('=') = self.peek_char() {
                        self.read_char();
                        Token::PlusEq
                    } else {
                        Token::Plus
                    }
                }
                '-' => {
                    if let Some('=') = self.peek_char() {
                        self.read_char();
                        Token::MinusEq
                    } else {
                        Token::Minus
                    }
                }
                '*' => {
                    if let Some('=') = self.peek_char() {
                        self.read_char();
                        Token::AsteriskEq
                    } else {
                        Token::Asterisk
                    }
                }
                '/' => match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::SlashEq
                    }
                    Some('/') => {
                        self.skip_comment();
                        Token::Comment
                    }
                    _ => Token::from_str(token.to_string().as_str()).unwrap(),
                },
                '%' => {
                    if let Some('=') = self.peek_char() {
                        self.read_char();
                        Token::ModEq
                    } else {
                        Token::Mod
                    }
                }
                '&' => {
                    if let Some('&') = self.peek_char() {
                        self.read_char();
                        Token::And
                    } else {
                        Token::Illegal
                    }
                }
                '|' => {
                    if let Some('|') = self.peek_char() {
                        self.read_char();
                        Token::Or
                    } else {
                        Token::Illegal
                    }
                }
                ',' | ';' | ':' | '(' | ')' | '{' | '}' | '[' | ']' => {
                    Token::from_str(token.to_string().as_str()).unwrap()
                }
                _ => {
                    if token.is_alphabetic() || token == '_' {
                        let literal = self.read_identifier();
                        let typ = lookup_ident(literal);
                        return typ;
                    } else if token.is_numeric() {
                        let literal = self.read_number();
                        if let Some('.') = self.ch {
                            self.read_char();
                            let next = self.read_number();
                            let value: f64 =
                                format!("{}.{}", literal, next).parse().expect("not float");
                            return Token::Float(value);
                        } else {
                            let value: i64 = literal.parse().expect("not number");
                            return Token::Int(value);
                        }
                    } else {
                        return Token::Illegal;
                    }
                }
            }
        } else {
            Token::EOF
        };
        self.read_char();
        tok
    }
}

#[cfg(test)]
mod test {
    use crate::token::Token;

    use super::Lexer;

    #[test]
    fn next_token_should_work() {
        let input = r#"let aa = 10;
        let bb = 20;
        let f1 = fn(x) {
            x;
        };
        let add = fn(x, y) {
            x + y;
        };
        let r = add(aa, bb);
        if (aa == bb) {
            return true;
        } else {
            return false;
        }
        let c_c = "string";
        let _dd = 1.1;
        bb += 10;
        break - continue
        null
        "#;
        let mut lexer = Lexer::new(input);

        let tests = vec![
            Token::Let,
            Token::Ident("aa".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::SemiColon,
            Token::Let,
            Token::Ident("bb".to_string()),
            Token::Assign,
            Token::Int(20),
            Token::SemiColon,
            Token::Let,
            Token::Ident("f1".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::SemiColon,
            Token::Rbrace,
            Token::SemiColon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::SemiColon,
            Token::Rbrace,
            Token::SemiColon,
            Token::Let,
            Token::Ident("r".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("aa".to_string()),
            Token::Comma,
            Token::Ident("bb".to_string()),
            Token::Rparen,
            Token::SemiColon,
            Token::If,
            Token::Lparen,
            Token::Ident("aa".to_string()),
            Token::Eq,
            Token::Ident("bb".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::Bool(true),
            Token::SemiColon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::Bool(false),
            Token::SemiColon,
            Token::Rbrace,
            Token::Let,
            Token::Ident("c_c".to_string()),
            Token::Assign,
            Token::String("string".to_string()),
            Token::SemiColon,
            Token::Let,
            Token::Ident("_dd".to_string()),
            Token::Assign,
            Token::Float(1.1),
            Token::SemiColon,
            Token::Ident("bb".to_string()),
            Token::PlusEq,
            Token::Int(10),
            Token::SemiColon,
            Token::Break,
            Token::Minus,
            Token::Continue,
            Token::Null,
            Token::EOF,
        ];
        tests.into_iter().for_each(|test| {
            let tok = lexer.next_token();
            assert_eq!(test, tok);
        })
    }

    #[test]
    fn is_alphabetic_should_work() {
        assert!(char::is_alphabetic('a'));
        assert!(!char::is_alphabetic('1'));
        assert!(!char::is_alphabetic(';'));
    }

    #[test]
    fn is_numeric_should_work() {
        assert!(char::is_numeric('1'));
        assert!(!char::is_numeric(';'));
    }
}
