use crate::token::{lookup_ident, Token, TokenError};
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

    fn read_string(&mut self) -> Option<String> {
        let pos = self.pos + 1;
        loop {
            self.read_char();
            if self.ch? == '"' {
                break;
            }
        }
        Some(self.input[pos..self.pos].to_string())
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
                '>' => match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::GtEq
                    }
                    Some('>') => {
                        self.read_char();
                        if let Some('=') = self.peek_char() {
                            self.read_char();
                            Token::RightShiftEq
                        } else {
                            Token::RightShift
                        }
                    }
                    _ => Token::Gt,
                },
                '<' => match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::LtEq
                    }
                    Some('<') => {
                        self.read_char();
                        if let Some('=') = self.peek_char() {
                            self.read_char();
                            Token::LeftShiftEq
                        } else {
                            Token::LeftShift
                        }
                    }
                    _ => Token::Lt,
                },
                '"' => {
                    if let Some(s) = self.read_string() {
                        Token::String(s)
                    } else {
                        Token::Illegal(TokenError::UnterminatedString)
                    }
                }
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
                        self.read_char();
                        // a comment goes until the end of the line.
                        while let Some(ch) = self.ch {
                            if ch == '\n' {
                                break;
                            }
                            self.read_char();
                        }
                        // do not need to call read_char in this recursion
                        return self.next_token();
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
                '&' => match self.peek_char() {
                    Some('&') => {
                        self.read_char();
                        Token::And
                    }
                    Some('=') => {
                        self.read_char();
                        Token::BitAndEq
                    }
                    _ => Token::BitAnd,
                },
                '|' => match self.peek_char() {
                    Some('|') => {
                        self.read_char();
                        Token::Or
                    }
                    Some('=') => {
                        self.read_char();
                        Token::BitOrEq
                    }
                    _ => Token::BitOr,
                },
                '^' => match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::BitXorEq
                    }
                    _ => Token::BitXor,
                },
                ',' | ';' | ':' | '(' | ')' | '{' | '}' | '[' | ']' => {
                    Token::from_str(token.to_string().as_str()).unwrap()
                }
                other => {
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
                        Token::Illegal(crate::token::TokenError::UnknowToken(other))
                    }
                }
            }
        } else {
            Token::Eof
        };
        self.read_char();
        tok
    }

    pub fn iter<'b>(&'b mut self) -> Iter<'a, 'b> {
        Iter {
            inner: self,
            end: false,
        }
    }
}

pub struct Iter<'a, 'b> {
    inner: &'b mut Lexer<'a>,
    end: bool,
}

impl<'a, 'b> Iterator for Iter<'a, 'b> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.end {
            return None;
        }
        let token = self.inner.next_token();
        if let Token::Eof = token {
            self.end = true
        }
        Some(token)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_float_issue() {
        // this test should failed
        let input = "1.2.3.";

        let mut lexer = Lexer::new(input);

        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            tokens.push(token.clone());
            if token == Token::Eof {
                break;
            }
        }

        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], Token::Float(1.2));
        assert_eq!(tokens[1], Token::Illegal(TokenError::UnknowToken('.')));
        assert_eq!(tokens[2], Token::Float(3.));
        assert_eq!(tokens[3], Token::Eof);
    }

    #[test]
    fn unterminated_string_should_failed() {
        let input = r#""This string is not terminated"#;
        let mut lexer = Lexer::new(input);

        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            tokens.push(token.clone());
            if token == Token::Eof {
                break;
            }
        }

        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], Token::Illegal(TokenError::UnterminatedString));
        assert_eq!(tokens[1], Token::Eof);
    }

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
            Token::Eof,
        ];
        tests.into_iter().for_each(|test| {
            let tok = lexer.next_token();
            assert_eq!(test, tok);
        })
    }

    #[test]
    fn test_lexer_iter() {
        let input = "let aa = 10;";
        let mut lexer = Lexer::new(input);

        let expect = vec![
            Token::Let,
            Token::Ident("aa".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::SemiColon,
            Token::Eof,
        ];
        let res: Vec<Token> = lexer.iter().collect();
        assert_eq!(expect, res);
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
