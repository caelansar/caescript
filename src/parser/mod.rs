use std::collections::hash_map::HashMap;

use crate::{ast, lexer, token};

type PrefixParseFn = fn(&mut Parser) -> Box<dyn ast::Expression>;
type InfixParseFn = fn(&mut Parser, Box<dyn ast::Expression>) -> Box<dyn ast::Expression>;

#[derive(PartialEq, PartialOrd)]
pub(crate) enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

impl From<token::TokenType> for Precedence {
    fn from(value: token::TokenType) -> Self {
        match value {
            token::TokenType::Eq => Precedence::Equals,
            token::TokenType::Ne => Precedence::Equals,
            token::TokenType::Lt => Precedence::LessGreater,
            token::TokenType::Gt => Precedence::LessGreater,
            token::TokenType::Plus => Precedence::Sum,
            token::TokenType::Minus => Precedence::Sum,
            token::TokenType::Slash => Precedence::Product,
            token::TokenType::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

struct Parser {
    lexer: lexer::Lexer,
    current_token: Option<token::Token>,
    peek_token: Option<token::Token>,
    errors: Vec<String>,
    prefix_parse_fn: HashMap<token::TokenType, PrefixParseFn>,
    infix_parse_fn: HashMap<token::TokenType, InfixParseFn>,
}

impl Parser {
    fn new(lexer: lexer::Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: None,
            peek_token: None,
            errors: Vec::new(),
            prefix_parse_fn: HashMap::new(),
            infix_parse_fn: HashMap::new(),
        };

        parser.register_prefix_parse_fn(token::TokenType::Ident, Parser::parse_identifier);
        parser.register_prefix_parse_fn(token::TokenType::Int, Parser::parse_integer_literal);
        parser.register_prefix_parse_fn(token::TokenType::True, Parser::parse_boolean_literal);
        parser.register_prefix_parse_fn(token::TokenType::False, Parser::parse_boolean_literal);
        parser.register_prefix_parse_fn(token::TokenType::Minus, Parser::parse_prefix_expression);
        parser.register_prefix_parse_fn(token::TokenType::Bang, Parser::parse_prefix_expression);
        parser.register_prefix_parse_fn(token::TokenType::Lparen, Parser::parse_grouped_expression);

        parser.register_infix_parse_fn(token::TokenType::Plus, Parser::parse_infix_expression);
        parser.register_infix_parse_fn(token::TokenType::Minus, Parser::parse_infix_expression);
        parser.register_infix_parse_fn(token::TokenType::Eq, Parser::parse_infix_expression);
        parser.register_infix_parse_fn(token::TokenType::Ne, Parser::parse_infix_expression);
        parser.register_infix_parse_fn(token::TokenType::Gt, Parser::parse_infix_expression);
        parser.register_infix_parse_fn(token::TokenType::Lt, Parser::parse_infix_expression);
        parser.register_infix_parse_fn(token::TokenType::Slash, Parser::parse_infix_expression);
        parser.register_infix_parse_fn(token::TokenType::Asterisk, Parser::parse_infix_expression);

        parser.next_token();
        parser.next_token();
        parser
    }

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, token: &token::TokenType) {
        self.errors.push(format!(
            "expect next_token to be {}, got {} instead",
            token,
            self.peek_token.as_ref().unwrap().typ
        ))
    }

    fn expect_peek(&mut self, token: &token::TokenType) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn current_precedence(&self) -> Precedence {
        self.current_token.as_ref().unwrap().typ.into()
    }

    fn peek_precedence(&self) -> Precedence {
        self.peek_token.as_ref().unwrap().typ.into()
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
    }

    fn register_prefix_parse_fn(&mut self, token: token::TokenType, parse_fn: PrefixParseFn) {
        self.prefix_parse_fn.insert(token, parse_fn);
    }

    fn register_infix_parse_fn(&mut self, token: token::TokenType, parse_fn: InfixParseFn) {
        self.infix_parse_fn.insert(token, parse_fn);
    }

    fn parse_infix_expression(
        &mut self,
        lhs: Box<dyn ast::Expression>,
    ) -> Box<dyn ast::Expression> {
        let precedence = self.current_precedence();
        let current_token = self.current_token.clone().unwrap();
        let operator = current_token.literal.clone();

        self.next_token();

        let rhs = self.parse_expression(precedence);

        Box::new(ast::InfixExpression::new(
            current_token,
            lhs,
            operator,
            rhs.unwrap(),
        ))
    }

    fn parse_grouped_expression(&mut self) -> Box<dyn ast::Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(&token::TokenType::Rparen) {
            println!("paren not match")
        }

        exp.unwrap()
    }

    fn parse_prefix_expression(&mut self) -> Box<dyn ast::Expression> {
        let current_token = self.current_token.clone().unwrap();
        let operator = current_token.literal.clone();

        self.next_token();

        let rhs = self.parse_expression(Precedence::Prefix);

        Box::new(ast::PrefixExpression::new(
            current_token,
            operator,
            rhs.unwrap(),
        ))
    }

    fn parse_identifier(&mut self) -> Box<dyn ast::Expression> {
        let tok = self.current_token.clone().unwrap();
        let literal = tok.clone().literal;
        Box::new(ast::Identifier::new(tok, literal))
    }

    fn parse_integer_literal(&mut self) -> Box<dyn ast::Expression> {
        let value: i64 = self
            .current_token
            .as_ref()
            .unwrap()
            .literal
            .as_str()
            .parse()
            .expect("not number");

        Box::new(ast::Literal::new(
            self.current_token.clone().unwrap(),
            value,
        ))
    }

    fn parse_boolean_literal(&mut self) -> Box<dyn ast::Expression> {
        let value = self.current_token_is(token::TokenType::True);

        Box::new(ast::Literal::new(
            self.current_token.clone().unwrap(),
            value,
        ))
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
                token::TokenType::Return => Some(Box::new(self.parse_return_statement())),
                _ => Some(Box::new(self.parse_expression_statement())),
            }
        } else {
            None
        }
    }

    fn parse_return_statement(&mut self) -> impl ast::Statement {
        let tok = self.current_token.take();
        let stmt = ast::ReturnStatement::new(tok.unwrap());

        self.next_token();

        while !self.current_token_is(token::TokenType::SemiColon) {
            self.next_token();
        }
        stmt
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let tok = self.current_token.take();
        if !self.expect_peek(&token::TokenType::Ident) {
            return None;
        }
        let curr_tok = self.current_token.take().unwrap();
        let identifier = ast::Identifier::new(curr_tok.clone(), curr_tok.literal.clone());
        let stmt = ast::LetStatement::new(tok.unwrap(), identifier);

        if !self.expect_peek(&token::TokenType::Assign) {
            return None;
        }

        while !self.current_token_is(token::TokenType::SemiColon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expression_statement(&mut self) -> impl ast::Statement {
        let expression = self.parse_expression(Precedence::Lowest);
        let stmt = ast::ExpressionStatement::new(self.current_token.clone().unwrap(), expression);

        while !self.current_token_is(token::TokenType::SemiColon)
            && !self.current_token_is(token::TokenType::EOF)
        {
            self.next_token();
        }
        stmt
    }

    fn no_prefix_parse_fn_error(&mut self, token: &token::TokenType) {
        self.errors
            .push(format!("no prefix parse fn found for {}", token))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn ast::Expression>> {
        let prefix_parse_fn = self
            .prefix_parse_fn
            .get(&self.current_token.clone().unwrap().typ);
        match prefix_parse_fn {
            Some(f) => {
                let mut lhs = f(self);

                while !self.peek_token_is(&token::TokenType::SemiColon)
                    && precedence < self.peek_precedence()
                {
                    let ty = &self.peek_token.as_ref().unwrap().typ;
                    let infix_parse_fn = self.infix_parse_fn.clone();
                    match infix_parse_fn.get(ty) {
                        Some(f) => {
                            self.next_token();

                            lhs = f(self, lhs);
                        }
                        None => {
                            return Some(lhs);
                        }
                    }
                }

                Some(lhs)
            }
            None => {
                self.no_prefix_parse_fn_error(&self.current_token.clone().unwrap().typ);
                None
            }
        }
    }

    fn current_token_is(&self, tok: token::TokenType) -> bool {
        self.current_token
            .as_ref()
            .map(|x| x.typ == tok)
            .is_some_and(|x| x)
    }

    fn peek_token_is(&self, tok: &token::TokenType) -> bool {
        self.peek_token
            .as_ref()
            .map(|x| &x.typ == tok)
            .is_some_and(|x| x)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast,
        ast::{ExpressionStatement, Identifier, Literal, Node, PrefixExpression},
        lexer,
    };

    use super::{Parser, Precedence};

    macro_rules! assert_expression {
        ($stmt:ident,$typ:ty,$exp_typ:ty,$val:literal) => {
            let stmt = $stmt.as_any().downcast_ref::<$typ>();
            assert!(
                stmt.is_some(),
                "stmt ({}) should be ExpressionStatement",
                &$stmt
            );

            let exp = stmt
                .unwrap()
                .expression
                .as_ref()
                .and_then(|exp| {
                    exp.as_any()
                        .downcast_ref::<$exp_typ>()
                        .and_then(|exp| Some(exp))
                })
                .unwrap();

            assert_eq!(
                $val, exp.value,
                "exp.value should be {}, got {}",
                $val, exp.value
            );

            assert_eq!(
                $stmt.token_literal(),
                $val.to_string(),
                "token_literal should be {}, got {}",
                $val,
                $stmt.token_literal()
            );
        };
    }

    macro_rules! assert_prefix_expression {
        ($stmt:ident,$typ:ty,$exp_typ:ty,$operator:literal,$rhs_typ:ty,$rhs_val:literal) => {
            let stmt = $stmt.as_any().downcast_ref::<$typ>();
            assert!(
                stmt.is_some(),
                "stmt ({}) should be ExpressionStatement",
                &$stmt
            );

            let exp = stmt
                .unwrap()
                .expression
                .as_ref()
                .and_then(|exp| {
                    exp.as_any()
                        .downcast_ref::<$exp_typ>()
                        .and_then(|exp| Some(exp))
                })
                .unwrap();

            assert_eq!($operator, exp.operator);

            let rhs = exp
                .rhs
                .as_ref()
                .as_any()
                .downcast_ref::<$rhs_typ>()
                .unwrap();

            assert_eq!(
                rhs.value, $rhs_val,
                "rhs.value should be {}, got {}",
                $rhs_val, rhs.value
            );

            assert_eq!(
                rhs.token_literal(),
                $rhs_val.to_string(),
                "token_literal should be {}, got {}",
                $rhs_val,
                rhs.token_literal()
            );
        };
    }

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

        let let_stmt = stmt.as_any().downcast_ref::<ast::LetStatement>();
        assert!(let_stmt.is_some(), "stmt should be LetStatement");

        assert!(
            let_stmt.unwrap().name.value == name,
            "name.value expect to be {}, got {} instead",
            name,
            let_stmt.unwrap().name.value,
        );

        assert!(
            let_stmt.unwrap().name.token_literal() == name,
            "name.token_literal expect to be {}, got {} instead",
            name,
            let_stmt.unwrap().name.token_literal(),
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
            let let_stmt = x.as_any().downcast_ref::<ast::ReturnStatement>();
            assert!(let_stmt.is_some(), "stmt should be ReturnStatement");

            assert!(
                x.token_literal() == "return".to_string(),
                "token_literal should be `return`"
            )
        })
    }

    #[test]
    fn identifier_expression_should_work() {
        let input = "cae;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(1, program.statements.len());

        program.statements.iter().for_each(|x| {
            assert_expression!(x, ExpressionStatement, Identifier, "cae");
        })
    }

    #[test]
    fn integer_expression_should_work() {
        let input = "4;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(1, program.statements.len());

        program.statements.iter().for_each(|x| {
            assert_expression!(x, ExpressionStatement, Literal<i64>, 4);
        })
    }

    #[test]
    fn boolean_expression_should_work() {
        let input = "false;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(1, program.statements.len());

        program.statements.iter().for_each(|x| {
            assert_expression!(x, ExpressionStatement, Literal<bool>, false);
        })
    }

    #[test]
    fn minus_expression_should_work() {
        let input = "-5;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(1, program.statements.len());

        program.statements.iter().for_each(|x| {
            assert_prefix_expression!(
                x,
                ExpressionStatement,
                PrefixExpression,
                "-",
                Literal<i64>,
                5
            );
        })
    }

    #[test]
    fn bang_expression_should_work() {
        let input = "!true;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(1, program.statements.len());

        program.statements.iter().for_each(|x| {
            assert_prefix_expression!(
                x,
                ExpressionStatement,
                PrefixExpression,
                "!",
                Literal<bool>,
                true
            );
        })
    }

    #[test]
    fn operator_precedence_parse_should_work() {
        let testdata: &[(&str, &str)] = &[
            ("-a + b;", "((-a) + b)"),
            ("a + b + c;", "((a + b) + c)"),
            ("a + b * c;", "(a + (b * c))"),
            ("a + b / c", "(a + (b / c))"),
            ("5 > 4", "(5 > 4)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 != 4", "(5 != 4)"),
            ("(a + b) * c", "((a + b) * c)"),
        ];

        testdata.iter().for_each(|testcase| {
            let lexer = lexer::Lexer::new(testcase.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parse_error(&parser);

            assert_eq!(testcase.1, &program.to_string())
        })
    }

    #[test]
    fn precedence_compare_should_work() {
        assert!(Precedence::Lowest < Precedence::Equals);
        assert!(Precedence::Equals < Precedence::LessGreater);
        assert!(Precedence::LessGreater < Precedence::Sum);
        assert!(Precedence::Product < Precedence::Call);
    }
}
