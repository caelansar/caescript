mod trace;

#[cfg(feature = "trace")]
use crate::defer;
use crate::{ast, lexer, token};
#[cfg(feature = "trace")]
use trace::{trace, untrace, ScopeCall};

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

impl From<token::Token> for Precedence {
    fn from(value: token::Token) -> Self {
        match value {
            token::Token::Eq => Precedence::Equals,
            token::Token::Ne => Precedence::Equals,
            token::Token::Lt => Precedence::LessGreater,
            token::Token::Gt => Precedence::LessGreater,
            token::Token::Plus => Precedence::Sum,
            token::Token::Minus => Precedence::Sum,
            token::Token::Slash => Precedence::Product,
            token::Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

struct Parser {
    lexer: lexer::Lexer,
    current_token: token::Token,
    next_token: token::Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(lexer: lexer::Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: token::Token::EOF,
            next_token: token::Token::EOF,
            errors: Vec::new(),
        };

        parser.next_token();
        parser.next_token();
        parser
    }

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn next_error(&mut self, token: &token::Token) {
        self.errors.push(format!(
            "expect next_token to be {}, got {} instead",
            token, self.next_token
        ))
    }

    fn expect_next(&mut self, token: &token::Token) -> bool {
        if self.next_token_is(token) {
            self.next_token();
            true
        } else {
            self.next_error(token);
            false
        }
    }

    fn current_precedence(&self) -> Precedence {
        self.current_token.clone().into()
    }

    fn next_precedence(&self) -> Precedence {
        self.next_token.clone().into()
    }

    fn next_token(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    fn parse_block_statemnt(&mut self) -> ast::BlockStatement {
        let current_token = self.current_token.clone();
        let mut statements = Vec::new();

        self.next_token();

        while !self.current_token_is(&token::Token::Rbrace) {
            if self.current_token_is(&token::Token::EOF) {
                panic!("unterminated block statement");
            }
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                statements.push(stmt)
            }
            self.next_token();
        }

        ast::BlockStatement::new(current_token, statements)
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_if_expression"));
        let current_token = self.current_token.clone();

        if !self.expect_next(&token::Token::Lparen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest);

        if !self.expect_next(&token::Token::Rparen) {
            return None;
        }
        if !self.expect_next(&token::Token::Lbrace) {
            return None;
        }

        let consequence = self.parse_block_statemnt();

        let mut alternative = None;

        if self.next_token_is(&token::Token::Else) {
            self.next_token();
            if self.expect_next(&token::Token::Lbrace) {
                alternative = Some(self.parse_block_statemnt());
            }
        }

        Some(Box::new(ast::IfExpression::new(
            current_token,
            condition.unwrap(),
            consequence,
            alternative,
        )))
    }

    fn parse_infix_expression(
        &mut self,
        lhs: Option<Box<dyn ast::Expression>>,
    ) -> Option<Box<dyn ast::Expression>> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_infix_expression"));
        if lhs.is_none() {
            return None;
        }

        let precedence = self.current_precedence();
        let current_token = self.current_token.clone();
        let operator = current_token.to_string();

        self.next_token();

        let rhs = self.parse_expression(precedence);

        Some(Box::new(ast::InfixExpression::new(
            current_token,
            lhs.unwrap(),
            operator,
            rhs.unwrap(),
        )))
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_grouped_expression"));
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_next(&token::Token::Rparen) {
            return None;
        }

        Some(exp.unwrap())
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_prefix_expression"));
        let current_token = self.current_token.clone();
        let operator = current_token.to_string();

        self.next_token();

        let rhs = self.parse_expression(Precedence::Prefix);

        Some(Box::new(ast::PrefixExpression::new(
            current_token,
            operator,
            rhs.unwrap(),
        )))
    }

    fn parse_identifier(&mut self) -> Option<Box<dyn ast::Expression>> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_identifier"));
        if let token::Token::Ident(ref ident) = self.current_token {
            Some(Box::new(ast::Identifier::new(
                self.current_token.clone(),
                ident.clone(),
            )))
        } else {
            None
        }
    }

    fn parse_string_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_string_literal"));
        if let token::Token::String(ref s) = self.current_token {
            Some(Box::new(ast::Literal::new(
                self.current_token.clone(),
                s.clone(),
            )))
        } else {
            None
        }
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_integer_literal"));
        if let token::Token::Int(int) = self.current_token {
            Some(Box::new(ast::Literal::new(self.current_token.clone(), int)))
        } else {
            None
        }
    }

    fn parse_boolean_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_boolean_literal"));
        if let token::Token::Bool(b) = self.current_token {
            Some(Box::new(ast::Literal::new(self.current_token.clone(), b)))
        } else {
            None
        }
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut stmts = Vec::new();
        while self.current_token != token::Token::EOF {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                stmts.push(stmt);
            }
            self.next_token();
        }
        ast::Program::new(stmts)
    }

    fn parse_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        match self.current_token {
            token::Token::Let => self
                .parse_let_statement()
                .and_then(|x| Some(Box::new(x) as Box<dyn ast::Statement>)),
            token::Token::Return => Some(Box::new(self.parse_return_statement())),
            _ => Some(Box::new(self.parse_expression_statement())),
        }
    }

    fn parse_return_statement(&mut self) -> impl ast::Statement {
        let stmt = ast::ReturnStatement::new(self.current_token.clone());

        self.next_token();

        while !self.current_token_is(&token::Token::SemiColon) {
            self.next_token();
        }
        stmt
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let tok = self.current_token.clone();

        match self.next_token {
            token::Token::Ident(_) => self.next_token(),
            _ => return None,
        };

        let identifier = match self.current_token {
            token::Token::Ident(ref ident) => {
                ast::Identifier::new(self.current_token.clone(), ident.clone())
            }
            _ => return None,
        };

        let stmt = ast::LetStatement::new(tok, identifier);

        if !self.expect_next(&token::Token::Assign) {
            return None;
        }

        while !self.current_token_is(&token::Token::SemiColon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expression_statement(&mut self) -> impl ast::Statement {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_expression_statement"));
        let expression = self.parse_expression(Precedence::Lowest);
        let stmt = ast::ExpressionStatement::new(self.current_token.clone(), expression);

        // eat SemiColon
        while self.next_token_is(&token::Token::SemiColon) {
            self.next_token();
        }
        stmt
    }

    fn no_prefix_parse_fn_error(&mut self, token: &token::Token) {
        self.errors
            .push(format!("no prefix parse fn found for {}", token))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn ast::Expression>> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_expression"));

        let mut lhs = match self.current_token {
            token::Token::Ident(_) => self.parse_identifier(),
            token::Token::Int(_) => self.parse_integer_literal(),
            token::Token::Bool(_) => self.parse_boolean_literal(),
            token::Token::Minus => self.parse_prefix_expression(),
            token::Token::Bang => self.parse_prefix_expression(),
            token::Token::Lparen => self.parse_grouped_expression(),
            token::Token::If => self.parse_if_expression(),
            token::Token::String(_) => self.parse_string_literal(),
            _ => {
                self.no_prefix_parse_fn_error(&self.current_token.clone());
                return None;
            }
        };

        while !self.next_token_is(&token::Token::SemiColon) && precedence < self.next_precedence() {
            match self.next_token {
                token::Token::Plus
                | token::Token::Minus
                | token::Token::Eq
                | token::Token::Ne
                | token::Token::Gt
                | token::Token::Lt
                | token::Token::Slash
                | token::Token::Asterisk => {
                    self.next_token();
                    lhs = self.parse_infix_expression(lhs)
                }
                _ => return lhs,
            }
        }

        lhs
    }

    fn current_token_is(&self, tok: &token::Token) -> bool {
        self.current_token == *tok
    }

    fn next_token_is(&self, tok: &token::Token) -> bool {
        self.next_token == *tok
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast,
        ast::{
            ExpressionStatement, Identifier, IfExpression, InfixExpression, Literal, Node,
            PrefixExpression,
        },
        lexer,
    };

    use super::{Parser, Precedence};

    macro_rules! assert_exp {
        ($exp:ident,$val:literal) => {
            assert_eq!(
                $val, $exp.value,
                "exp.value should be {}, got {}",
                $val, $exp.value
            );
        };
    }

    macro_rules! assert_expression {
        ($stmt:expr,$exp_typ:ty,$val:literal) => {
            let stmt = $stmt.as_any().downcast_ref::<ExpressionStatement>();
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

            assert_exp!(exp, $val);
        };
    }

    macro_rules! assert_prefix_expression {
        ($stmt:ident,$operator:literal,$rhs_typ:ty,$rhs_val:literal) => {
            let stmt = $stmt.as_any().downcast_ref::<ExpressionStatement>();
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
                        .downcast_ref::<PrefixExpression>()
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

            assert_exp!(rhs, $rhs_val);
        };
    }

    macro_rules! assert_infix_expression {
        ($exp:expr,$lhs_typ:ty,$lhs_val:literal,$operator:literal,$rhs_typ:ty,$rhs_val:literal) => {
            let exp = $exp
                .as_any()
                .downcast_ref::<InfixExpression>()
                .and_then(|exp| Some(exp))
                .unwrap();

            assert_eq!($operator, exp.operator);

            let lhs = exp
                .lhs
                .as_ref()
                .as_any()
                .downcast_ref::<$lhs_typ>()
                .unwrap();

            assert_exp!(lhs, $lhs_val);

            let rhs = exp
                .rhs
                .as_ref()
                .as_any()
                .downcast_ref::<$rhs_typ>()
                .unwrap();

            assert_exp!(rhs, $rhs_val);
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

        let tests = vec!["a".to_string(), "b".to_string(), "c".to_string()];

        tests.into_iter().enumerate().for_each(|(idx, test)| {
            let stmt = &program.statements[idx];
            assert_let_statement(stmt, test);
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
        let let_stmt = stmt.as_any().downcast_ref::<ast::LetStatement>();
        assert!(let_stmt.is_some(), "stmt should be LetStatement");

        assert!(
            let_stmt.unwrap().name.value == name,
            "name.value expect to be {}, got {} instead",
            name,
            let_stmt.unwrap().name.value,
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
            assert_expression!(x, Identifier, "cae");
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
            assert_expression!(x, Literal<i64>, 4);
        })
    }

    #[test]
    fn string_expression_should_work() {
        let input = r#""abc""#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(1, program.statements.len());

        program.statements.iter().for_each(|x| {
            assert_expression!(x, Literal<String>, "abc");
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
            assert_expression!(x, Literal<bool>, false);
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
            assert_prefix_expression!(x, "-", Literal<i64>, 5);
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
            assert_prefix_expression!(x, "!", Literal<bool>, true);
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

    #[test]
    fn if_expression_should_work() {
        let input = "if (x < y) {x} else {y}";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];

        let stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();
        assert!(stmt.is_some(), "stmt should be ExpressionStatement",);

        let exp = stmt
            .unwrap()
            .expression
            .as_ref()
            .and_then(|exp| {
                exp.as_any()
                    .downcast_ref::<IfExpression>()
                    .and_then(|exp| Some(exp))
            })
            .unwrap();

        println!("{}", exp.to_string());

        assert_infix_expression!(exp.condition, Identifier, "x", "<", Identifier, "y");

        assert_eq!(1, exp.consequence.statements.len());

        assert_expression!(&exp.consequence.statements[0], Identifier, "x");

        assert!(exp.alternative.is_some());

        exp.alternative.as_ref().inspect(|x| {
            assert_expression!(x.statements[0], Identifier, "y");
        });
    }
}
