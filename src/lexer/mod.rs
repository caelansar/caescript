use crate::token::{Token, TokenType};
use std::str::FromStr;

#[derive(Default)]
struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    next_pos: usize,
    ch: Option<char>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            ..Default::default()
        };
        l.read_char();
        l
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
        while self.ch.is_some() && char::is_alphabetic(self.ch.unwrap()) {
            self.read_char();
        }
        return self.input[pos..self.pos].to_string();
    }

    fn read_number(&mut self) -> String {
        let pos = self.pos;
        while self.ch.is_some() && char::is_numeric(self.ch.unwrap()) {
            self.read_char();
        }
        return self.input[pos..self.pos].to_string();
    }

    fn eat_whitespace(&mut self) {
        while self.ch.is_some() && char::is_whitespace(self.ch.unwrap()) {
            self.read_char()
        }
    }

    pub(crate) fn next_token(&mut self) -> Token {
        self.eat_whitespace();

        let tok = if let Some(token) = self.ch {
            match token {
                '=' => Token::new(
                    TokenType::from_str(token.to_string().as_str()).unwrap(),
                    token.to_string(),
                ),
                ',' => Token::new(
                    TokenType::from_str(token.to_string().as_str()).unwrap(),
                    token.to_string(),
                ),
                ';' => Token::new(
                    TokenType::from_str(token.to_string().as_str()).unwrap(),
                    token.to_string(),
                ),
                '(' => Token::new(
                    TokenType::from_str(token.to_string().as_str()).unwrap(),
                    token.to_string(),
                ),
                ')' => Token::new(
                    TokenType::from_str(token.to_string().as_str()).unwrap(),
                    token.to_string(),
                ),
                '{' => Token::new(
                    TokenType::from_str(token.to_string().as_str()).unwrap(),
                    token.to_string(),
                ),
                '}' => Token::new(
                    TokenType::from_str(token.to_string().as_str()).unwrap(),
                    token.to_string(),
                ),
                '+' => Token::new(
                    TokenType::from_str(token.to_string().as_str()).unwrap(),
                    token.to_string(),
                ),
                _ => {
                    if char::is_alphabetic(token) {
                        let literal = self.read_identifier();
                        let typ = Token::lookup_ident(literal.clone());
                        return Token::new(typ, literal);
                    } else if char::is_numeric(token) {
                        let literal = self.read_number();
                        return Token::new(TokenType::Int, literal);
                    } else {
                        Token::new(TokenType::Illegal, token.to_string())
                    }
                }
            }
        } else {
            Token::new(TokenType::EOF, "".to_string())
        };
        self.read_char();
        tok
    }
}

#[cfg(test)]
mod test {
    use crate::token::TokenType;

    use super::Lexer;

    #[derive(Debug)]
    struct Testcase {
        expected_type: TokenType,
        expected_literal: String,
    }

    #[test]
    fn next_token_should_work() {
        let input = r#"let aa = 10;
        let bb = 20;
        let add = fn(x, y) {
            x + y;
        };
        let r = add(aa, bb);"#;
        let mut lexer = Lexer::new(input);

        let tests = vec![
            Testcase {
                expected_type: TokenType::Let,
                expected_literal: "let".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "aa".to_string(),
            },
            Testcase {
                expected_type: TokenType::Assign,
                expected_literal: "=".to_string(),
            },
            Testcase {
                expected_type: TokenType::Int,
                expected_literal: "10".to_string(),
            },
            Testcase {
                expected_type: TokenType::SimiColon,
                expected_literal: ";".to_string(),
            },
            Testcase {
                expected_type: TokenType::Let,
                expected_literal: "let".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "bb".to_string(),
            },
            Testcase {
                expected_type: TokenType::Assign,
                expected_literal: "=".to_string(),
            },
            Testcase {
                expected_type: TokenType::Int,
                expected_literal: "20".to_string(),
            },
            Testcase {
                expected_type: TokenType::SimiColon,
                expected_literal: ";".to_string(),
            },
            Testcase {
                expected_type: TokenType::Let,
                expected_literal: "let".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "add".to_string(),
            },
            Testcase {
                expected_type: TokenType::Assign,
                expected_literal: "=".to_string(),
            },
            Testcase {
                expected_type: TokenType::Function,
                expected_literal: "fn".to_string(),
            },
            Testcase {
                expected_type: TokenType::Lparen,
                expected_literal: "(".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "x".to_string(),
            },
            Testcase {
                expected_type: TokenType::Comma,
                expected_literal: ",".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "y".to_string(),
            },
            Testcase {
                expected_type: TokenType::Rparen,
                expected_literal: ")".to_string(),
            },
            Testcase {
                expected_type: TokenType::Lbrace,
                expected_literal: "{".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "x".to_string(),
            },
            Testcase {
                expected_type: TokenType::Plus,
                expected_literal: "+".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "y".to_string(),
            },
            Testcase {
                expected_type: TokenType::SimiColon,
                expected_literal: ";".to_string(),
            },
            Testcase {
                expected_type: TokenType::Rbrace,
                expected_literal: "}".to_string(),
            },
            Testcase {
                expected_type: TokenType::SimiColon,
                expected_literal: ";".to_string(),
            },
            Testcase {
                expected_type: TokenType::Let,
                expected_literal: "let".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "r".to_string(),
            },
            Testcase {
                expected_type: TokenType::Assign,
                expected_literal: "=".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "add".to_string(),
            },
            Testcase {
                expected_type: TokenType::Lparen,
                expected_literal: "(".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "aa".to_string(),
            },
            Testcase {
                expected_type: TokenType::Comma,
                expected_literal: ",".to_string(),
            },
            Testcase {
                expected_type: TokenType::Ident,
                expected_literal: "bb".to_string(),
            },
            Testcase {
                expected_type: TokenType::Rparen,
                expected_literal: ")".to_string(),
            },
            Testcase {
                expected_type: TokenType::SimiColon,
                expected_literal: ";".to_string(),
            },
            Testcase {
                expected_type: TokenType::EOF,
                expected_literal: "".to_string(),
            },
        ];
        tests.into_iter().for_each(|test| {
            let tok = lexer.next_token();
            println!("check token {:?}, expect {:?}", tok, test);
            assert_eq!(test.expected_type, tok.typ);
            assert_eq!(test.expected_literal, tok.literal);
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
