use crate::token::{lookup_ident, Token};
use std::str::FromStr;

#[derive(Default)]
pub(crate) struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    next_pos: usize,
    ch: Option<char>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input: input.as_ref(),
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
        if self.next_pos >= self.input.len() {
            self.ch = None
        } else {
            self.ch = Some(self.input.as_bytes()[self.next_pos] as char);
        }
        self.pos = self.next_pos;
        self.next_pos += 1;
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.pos;
        while let Some(true) = self.ch.map(|c| c.is_alphabetic()) {
            self.read_char();
        }
        return self.input[pos..self.pos].to_string();
    }

    fn read_number(&mut self) -> String {
        let pos = self.pos;
        while let Some(true) = self.ch.map(|c| c.is_numeric()) {
            self.read_char();
        }
        return self.input[pos..self.pos].to_string();
    }

    fn read_string(&mut self) -> String {
        let pos = self.pos + 1;
        loop {
            self.read_char();
            if self.ch.is_none() || self.ch.is_some_and(|x| x == '"') {
                break;
            }
        }
        return self.input[pos..self.pos].to_string();
    }

    #[inline(always)]
    fn eat_whitespace(&mut self) {
        while let Some(true) = self.ch.map(|c| c.is_whitespace()) {
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
                '"' => Token::String(self.read_string()),
                ',' | ';' | '(' | ')' | '{' | '}' | '+' | '-' | '*' | '/' | '>' | '<' => {
                    Token::from_str(token.to_string().as_str()).unwrap()
                }
                _ => {
                    if token.is_alphabetic() {
                        let literal = self.read_identifier();
                        let typ = lookup_ident(literal.clone());
                        return typ;
                    } else if token.is_numeric() {
                        let literal = self.read_number();
                        let value: i64 = literal.as_str().parse().expect("not number");
                        return Token::Int(value);
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
        let add = fn(x, y) {
            x + y;
        };
        let r = add(aa, bb);
        if (aa == bb) {
            return true;
        } else {
            return false;
        }
        let cc = "string";
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
            Token::Ident("cc".to_string()),
            Token::Assign,
            Token::String("string".to_string()),
            Token::SemiColon,
            Token::EOF,
        ];
        tests.into_iter().for_each(|test| {
            let tok = lexer.next_token();
            println!("check token {:?}, expect {:?}", tok, test);
            assert_eq!(test, tok);
            // assert_eq!(test.expected_literal, tok.literal);
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
