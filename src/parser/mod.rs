use crate::{ast, lexer, token};

struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    current_token: Option<token::Token>,
    peek_token: Option<token::Token>,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: lexer::Lexer<'a>) -> Self {
        let mut parser = Self {
            lexer,
            current_token: None,
            peek_token: None,
            errors: Vec::new(),
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, tok: token::TokenType) {
        self.errors.push(format!(
            "expect next_token to be {}, got {} instead",
            tok,
            self.peek_token.as_ref().unwrap().typ
        ))
    }

    fn expect_peek(&mut self, tok: token::TokenType) -> bool {
        if self.peek_token_is(tok) {
            self.next_token();
            true
        } else {
            self.peek_error(tok);
            false
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut stmts = Vec::new();
        while self
            .current_token
            .as_ref()
            .is_some_and(|x| x.typ != token::TokenType::EOF)
        {
            let stmt = self.parse_statement();
            if stmt.is_some() {
                stmts.push(stmt.unwrap());
            }
            self.next_token();
        }
        ast::Program::new(stmts)
    }

    fn parse_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        if let Some(ref tok) = self.current_token {
            match tok.typ {
                token::TokenType::Let => self
                    .parse_let_statement()
                    .and_then(|x| Some(Box::new(x) as Box<dyn ast::Statement>)),
                token::TokenType::Return => self
                    .parse_return_statement()
                    .and_then(|x| Some(Box::new(x) as Box<dyn ast::Statement>)),
                _ => None,
            }
        } else {
            None
        }
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        let tok = self.current_token.take();
        let stmt = ast::ReturnStatement::new(tok.unwrap());

        self.next_token();

        while !self.current_token_is(token::TokenType::SemiColon) {
            self.next_token();
        }
        Some(stmt)
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let tok = self.current_token.take();
        if !self.expect_peek(token::TokenType::Ident) {
            return None;
        }
        let curr_tok = self.current_token.take().unwrap();
        let identifier = ast::Identifier::new(curr_tok.clone(), curr_tok.literal.clone());
        let stmt = ast::LetStatement::new(tok.unwrap(), identifier);

        if !self.expect_peek(token::TokenType::Assign) {
            return None;
        }

        while !self.current_token_is(token::TokenType::SemiColon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn current_token_is(&self, tok: token::TokenType) -> bool {
        self.current_token
            .as_ref()
            .map(|x| x.typ == tok)
            .is_some_and(|x| x)
    }

    fn peek_token_is(&self, tok: token::TokenType) -> bool {
        self.peek_token
            .as_ref()
            .map(|x| x.typ == tok)
            .is_some_and(|x| x)
    }
}

#[cfg(test)]
mod test {
    use crate::{ast, ast::Node, lexer};

    use super::Parser;

    #[test]
    fn let_statement_should_work() {
        let input = r#"
            let a = 1;
            let b = 2;
            let c = 3;
            "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(3, program.statements.len());

        #[derive(Debug)]
        struct Testcase {
            expected_identifier: String,
        }

        let tests = vec![
            Testcase {
                expected_identifier: "a".to_string(),
            },
            Testcase {
                expected_identifier: "b".to_string(),
            },
            Testcase {
                expected_identifier: "c".to_string(),
            },
        ];
        tests.into_iter().enumerate().for_each(|(idx, test)| {
            let stmt = &program.statements[idx];
            assert_let_statement(stmt, test.expected_identifier);
        })
    }

    fn check_parse_error(parser: &Parser) {
        let errs = parser.errors();

        for err in errs.iter() {
            eprintln!("parse error: {}", err)
        }

        assert!(errs.len() == 0)
    }

    fn assert_let_statement(stmt: &Box<dyn ast::Statement>, name: String) {
        assert!(
            stmt.token_literal() == "let",
            "expect to be {}, got {} instead",
            "let",
            stmt.token_literal()
        );

        assert!(stmt.name().is_some(), "let stmt should have name");

        assert!(
            stmt.name().unwrap().value == name,
            "name.value expect to be {}, got {} instead",
            name,
            stmt.name().unwrap().value,
        );

        assert!(
            stmt.name().unwrap().token_literal() == name,
            "name.token_literal expect to be {}, got {} instead",
            name,
            stmt.name().unwrap().token_literal(),
        );
    }

    #[test]
    fn return_statement_should_work() {
        let input = r#"
            return 1;
            return 2;
            return 3;
            "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(3, program.statements.len());

        program.statements.iter().for_each(|x| {
            assert!(
                x.token_literal() == "return".to_string(),
                "token_literal should be `return`"
            )
        })
    }
}
