use crate::token::{Token, TokenType};
use std::str::FromStr;

#[derive(Default)]
pub(crate) struct Lexer {
    input: String,
    pos: usize,
    next_pos: usize,
    ch: Option<char>,
}

impl Lexer {
    pub(crate) fn new(input: impl AsRef<str>) -> Self {
        let mut l = Lexer {
            input: input.as_ref().to_string(),
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
                        Token::new(TokenType::Eq, "==".to_string())
                    } else {
                        Token::new(
                            TokenType::from_str(token.to_string().as_str()).unwrap(),
                            token.to_string(),
                        )
                    }
                }
                '!' => {
                    if let Some('=') = self.peek_char() {
                        self.read_char();
                        Token::new(TokenType::Ne, "!=".to_string())
                    } else {
                        Token::new(
                            TokenType::from_str(token.to_string().as_str()).unwrap(),
                            token.to_string(),
                        )
                    }
                }
                ',' | ';' | '(' | ')' | '{' | '}' | '+' | '-' | '*' | '/' | '>' | '<' => {
                    Token::new(
                        TokenType::from_str(token.to_string().as_str()).unwrap(),
                        token.to_string(),
                    )
                }
                _ => {
                    if token.is_alphabetic() {
                        let literal = self.read_identifier();
                        let typ = Token::lookup_ident(literal.clone());
                        return Token::new(typ, literal);
                    } else if token.is_numeric() {
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
        let r = add(aa, bb);
        if (aa == bb) {
            return true;
        } else {
            return false;
        }
        "#;
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
                expected_type: TokenType::SemiColon,
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
                expected_type: TokenType::SemiColon,
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
                expected_type: TokenType::SemiColon,
                expected_literal: ";".to_string(),
            },
            Testcase {
                expected_type: TokenType::Rbrace,
                expected_literal: "}".to_string(),
            },
            Testcase {
                expected_type: TokenType::SemiColon,
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
                expected_type: TokenType::SemiColon,
                expected_literal: ";".to_string(),
            },
            Testcase {
                expected_type: TokenType::If,
                expected_literal: "if".to_string(),
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
                expected_type: TokenType::Eq,
                expected_literal: "==".to_string(),
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
                expected_type: TokenType::Lbrace,
                expected_literal: "{".to_string(),
            },
            Testcase {
                expected_type: TokenType::Return,
                expected_literal: "return".to_string(),
            },
            Testcase {
                expected_type: TokenType::True,
                expected_literal: "true".to_string(),
            },
            Testcase {
                expected_type: TokenType::SemiColon,
                expected_literal: ";".to_string(),
            },
            Testcase {
                expected_type: TokenType::Rbrace,
                expected_literal: "}".to_string(),
            },
            Testcase {
                expected_type: TokenType::Else,
                expected_literal: "else".to_string(),
            },
            Testcase {
                expected_type: TokenType::Lbrace,
                expected_literal: "{".to_string(),
            },
            Testcase {
                expected_type: TokenType::Return,
                expected_literal: "return".to_string(),
            },
            Testcase {
                expected_type: TokenType::False,
                expected_literal: "false".to_string(),
            },
            Testcase {
                expected_type: TokenType::SemiColon,
                expected_literal: ";".to_string(),
            },
            Testcase {
                expected_type: TokenType::Rbrace,
                expected_literal: "}".to_string(),
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
