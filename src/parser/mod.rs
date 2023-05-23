mod trace;

#[cfg(feature = "trace")]
use crate::defer;
use crate::{ast, lexer, token};
#[cfg(feature = "trace")]
use trace::{trace, untrace, ScopeCall};

#[derive(PartialEq, PartialOrd, Debug)]
pub(crate) enum Precedence {
    Lowest,
    Assign,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl From<&token::Token> for Precedence {
    fn from(value: &token::Token) -> Self {
        match value {
            token::Token::Eq => Precedence::Equals,
            token::Token::Assign => Precedence::Assign,
            token::Token::PlusEq => Precedence::Assign,
            token::Token::MinusEq => Precedence::Assign,
            token::Token::AsteriskEq => Precedence::Assign,
            token::Token::SlashEq => Precedence::Assign,
            token::Token::Ne => Precedence::Equals,
            token::Token::Lt => Precedence::LessGreater,
            token::Token::Gt => Precedence::LessGreater,
            token::Token::LtEq => Precedence::LessGreater,
            token::Token::GtEq => Precedence::LessGreater,
            token::Token::Plus => Precedence::Sum,
            token::Token::Minus => Precedence::Sum,
            token::Token::Slash => Precedence::Product,
            token::Token::Asterisk => Precedence::Product,
            token::Token::Lparen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

pub(crate) struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    current_token: token::Token,
    next_token: token::Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(lexer: lexer::Lexer<'a>) -> Self {
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

    #[inline(always)]
    pub(crate) fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    #[inline(always)]
    fn next_error(&mut self, token: &token::Token) {
        self.errors.push(format!(
            "expect next_token to be {}, got {} instead",
            token, self.next_token
        ))
    }

    #[inline]
    fn expect_next(&mut self, token: &token::Token) -> bool {
        if self.next_token_is(token) {
            self.next_token();
            true
        } else {
            self.next_error(token);
            false
        }
    }

    #[inline(always)]
    fn current_precedence(&self) -> Precedence {
        (&self.current_token).into()
    }

    #[inline(always)]
    fn next_precedence(&self) -> Precedence {
        (&self.next_token).into()
    }

    #[inline(always)]
    fn next_token(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    #[inline(always)]
    fn parse_block_statemnt(&mut self) -> ast::BlockStatement {
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

        ast::BlockStatement(statements)
    }

    #[inline]
    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_if_expression"));

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

        Some(ast::Expression::If {
            condition: Box::new(condition.unwrap()),
            consequence,
            alternative,
        })
    }

    #[inline(always)]
    fn parse_infix_expression(&mut self, lhs: Option<ast::Expression>) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_infix_expression"));
        let infix: ast::Infix = match (&self.current_token).try_into() {
            Ok(infix) => infix,
            Err(_) => return None,
        };

        if lhs.is_none() {
            return None;
        }

        let precedence = self.current_precedence();

        self.next_token();

        let rhs = self.parse_expression(precedence);

        Some(ast::Expression::Infix(
            infix,
            Box::new(lhs.unwrap()),
            Box::new(rhs.unwrap()),
        ))
    }

    #[inline(always)]
    fn parse_grouped_expression(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_grouped_expression"));
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_next(&token::Token::Rparen) {
            return None;
        }

        Some(exp.unwrap())
    }

    #[inline(always)]
    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_prefix_expression"));
        let prefix = match (&self.current_token).try_into() {
            Ok(infix) => infix,
            Err(_) => return None,
        };

        self.next_token();

        let rhs = self.parse_expression(Precedence::Prefix);

        Some(ast::Expression::Prefix(prefix, Box::new(rhs.unwrap())))
    }

    #[inline(always)]
    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_identifier"));
        if let token::Token::Ident(ref ident) = self.current_token {
            Some(ast::Expression::Ident(ast::Ident(ident.clone())))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_string_literal(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_string_literal"));
        if let token::Token::String(ref s) = self.current_token {
            Some(ast::Expression::Literal(ast::Literal::String(s.clone())))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_integer_literal(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_integer_literal"));
        if let token::Token::Int(int) = self.current_token {
            Some(ast::Expression::Literal(ast::Literal::Int(int)))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_float_literal(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_integer_literal"));
        if let token::Token::Float(float) = self.current_token {
            Some(ast::Expression::Literal(ast::Literal::Float(float)))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_boolean_literal(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_boolean_literal"));
        if let token::Token::Bool(b) = self.current_token {
            Some(ast::Expression::Literal(ast::Literal::Bool(b)))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_function_literal(&mut self) -> Option<ast::Expression> {
        if !self.expect_next(&token::Token::Lparen) {
            return None;
        }

        let params = match self.parse_function_params() {
            Some(params) => params,
            None => return None,
        };

        if !self.expect_next(&token::Token::Lbrace) {
            return None;
        }

        let body = self.parse_block_statemnt();

        Some(ast::Expression::Func { params, body })
    }

    fn parse_function_params(&mut self) -> Option<Vec<ast::Ident>> {
        let mut idents = vec![];

        if self.next_token_is(&token::Token::Rparen) {
            self.next_token();
            return Some(idents);
        }

        self.next_token();

        match self.parse_identifier() {
            Some(ast::Expression::Ident(ident)) => idents.push(ident),
            _ => return None,
        };

        while self.next_token_is(&token::Token::Comma) {
            self.next_token();
            self.next_token();

            match self.parse_identifier() {
                Some(ast::Expression::Ident(ident)) => idents.push(ident),
                _ => return None,
            };
        }

        if !self.expect_next(&token::Token::Rparen) {
            return None;
        }

        Some(idents)
    }

    fn parse_assign_expression(
        &mut self,
        op: ast::Assign,
        lhs: Option<ast::Expression>,
    ) -> Option<ast::Expression> {
        if let Some(ast::Expression::Ident(ident)) = lhs {
            self.next_token();
            self.next_token();

            let expr = match self.parse_expression(Precedence::Lowest) {
                Some(expr) => expr,
                None => return None,
            };
            while !self.current_token_is(&token::Token::SemiColon)
                && !self.current_token_is(&token::Token::EOF)
            {
                self.next_token();
            }
            Some(ast::Expression::Assign(op, ident, Box::new(expr)))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_call_expression(&mut self, lhs: Option<ast::Expression>) -> Option<ast::Expression> {
        if lhs.is_none() {
            return None;
        }

        let args = match self.parse_call_args() {
            Some(args) => args,
            None => return None,
        };

        Some(ast::Expression::Call {
            func: Box::new(lhs.unwrap()),
            args,
        })
    }

    fn parse_call_args(&mut self) -> Option<Vec<ast::Expression>> {
        let mut args = vec![];

        if self.next_token_is(&token::Token::Rparen) {
            self.next_token();
            return Some(args);
        }

        self.next_token();

        args.push(self.parse_expression(Precedence::Lowest).unwrap());

        while self.next_token_is(&token::Token::Comma) {
            self.next_token();
            self.next_token();

            args.push(self.parse_expression(Precedence::Lowest).unwrap());
        }

        if !self.expect_next(&token::Token::Rparen) {
            return None;
        }

        Some(args)
    }

    pub(crate) fn parse_program(&mut self) -> ast::Program {
        let mut stmts = Vec::new();
        while self.current_token != token::Token::EOF {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                stmts.push(stmt);
            }
            self.next_token();
        }
        ast::BlockStatement(stmts)
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.current_token {
            token::Token::Let => self.parse_let_statement(),
            token::Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        self.next_token();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        self.next_token();

        while !self.current_token_is(&token::Token::SemiColon)
            && !self.current_token_is(&token::Token::EOF)
        {
            self.next_token();
        }
        Some(ast::Statement::Return(expr))
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        match self.next_token {
            token::Token::Ident(_) => self.next_token(),
            _ => return None,
        };

        let identifier = match self.current_token {
            token::Token::Ident(ref ident) => ast::Ident(ident.clone()),
            _ => return None,
        };

        if !self.expect_next(&token::Token::Assign) {
            return None;
        }

        self.next_token();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        while !self.current_token_is(&token::Token::SemiColon)
            && !self.current_token_is(&token::Token::EOF)
        {
            self.next_token();
        }

        Some(ast::Statement::Let(identifier, expr))
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_expression_statement"));
        let expression = self.parse_expression(Precedence::Lowest);
        let expression = match expression {
            Some(expression) => expression,
            None => return None,
        };
        let stmt = ast::Statement::Expression(expression);

        // eat SemiColon
        while self.next_token_is(&token::Token::SemiColon) {
            self.next_token();
        }
        Some(stmt)
    }

    #[inline(always)]
    fn no_prefix_parse_fn_error(&mut self, token: &token::Token) {
        self.errors
            .push(format!("no prefix parse fn found for {}", token))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace, trace("parse_expression"));

        let mut lhs = match self.current_token {
            token::Token::Ident(_) => self.parse_identifier(),
            token::Token::Int(_) => self.parse_integer_literal(),
            token::Token::Float(_) => self.parse_float_literal(),
            token::Token::Bool(_) => self.parse_boolean_literal(),
            token::Token::Minus => self.parse_prefix_expression(),
            token::Token::Bang => self.parse_prefix_expression(),
            token::Token::Lparen => self.parse_grouped_expression(),
            token::Token::If => self.parse_if_expression(),
            token::Token::String(_) => self.parse_string_literal(),
            token::Token::Function => self.parse_function_literal(),
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
                | token::Token::GtEq
                | token::Token::Lt
                | token::Token::LtEq
                | token::Token::Slash
                | token::Token::Asterisk => {
                    self.next_token();
                    lhs = self.parse_infix_expression(lhs)
                }
                token::Token::Lparen => {
                    self.next_token();
                    lhs = self.parse_call_expression(lhs)
                }
                token::Token::Assign
                | token::Token::PlusEq
                | token::Token::MinusEq
                | token::Token::AsteriskEq
                | token::Token::SlashEq => {
                    lhs = self.parse_assign_expression((&self.next_token).try_into().unwrap(), lhs)
                }
                _ => return lhs,
            }
        }

        lhs
    }

    #[inline(always)]
    fn current_token_is(&self, tok: &token::Token) -> bool {
        self.current_token == *tok
    }

    #[inline(always)]
    fn next_token_is(&self, tok: &token::Token) -> bool {
        self.next_token == *tok
    }
}

#[cfg(test)]
mod test {
    use crate::{ast, lexer};

    use super::{Parser, Precedence};

    fn check_parse_error(parser: &Parser) {
        let errs = parser.errors();

        for err in errs.iter() {
            eprintln!("parse error: {}", err)
        }

        assert!(errs.len() == 0)
    }

    #[test]
    fn assign_statement_should_work() {
        let input = r#"
            a += 1;
            "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![
                //     ast::Statement::Expression(ast::Expression::Assign(
                //         ast::Assign::Assign,
                //         ast::Ident("a".to_string()),
                //         Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                //     )),
                //     ast::Statement::Expression(ast::Expression::Assign(
                //         ast::Assign::Assign,
                //         ast::Ident("b".to_string()),
                //         Box::new(ast::Expression::Infix(
                //             ast::Infix::Plus,
                //             Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                //             Box::new(ast::Expression::Literal(ast::Literal::Int(2))),
                //         )),
                //     )),
                ast::Statement::Expression(ast::Expression::Assign(
                    ast::Assign::PlusEq,
                    ast::Ident("a".to_string()),
                    Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                )),
            ])
        );
    }

    #[test]
    fn let_statement_should_work() {
        let input = r#"
            let a = 1;
            let b = 2;
            let c = 3;
            let d = e
            "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![
                ast::Statement::Let(
                    ast::Ident("a".to_string()),
                    ast::Expression::Literal(ast::Literal::Int(1)),
                ),
                ast::Statement::Let(
                    ast::Ident("b".to_string()),
                    ast::Expression::Literal(ast::Literal::Int(2)),
                ),
                ast::Statement::Let(
                    ast::Ident("c".to_string()),
                    ast::Expression::Literal(ast::Literal::Int(3)),
                ),
                ast::Statement::Let(
                    ast::Ident("d".to_string()),
                    ast::Expression::Ident(ast::Ident("e".to_string())),
                ),
            ])
        );
    }

    #[test]
    fn return_statement_should_work() {
        let input = r#"
            return 1;
            return "2";
            return true;
            return a
            "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![
                ast::Statement::Return(ast::Expression::Literal(ast::Literal::Int(1))),
                ast::Statement::Return(ast::Expression::Literal(ast::Literal::String(
                    "2".to_string()
                ))),
                ast::Statement::Return(ast::Expression::Literal(ast::Literal::Bool(true))),
                ast::Statement::Return(ast::Expression::Ident(ast::Ident("a".to_string()))),
            ])
        );
    }

    #[test]
    fn identifier_expression_should_work() {
        let input = "cae;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Ident(
                ast::Ident("cae".to_string())
            ))])
        );
    }

    #[test]
    fn integer_expression_should_work() {
        let input = "4;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Literal(
                ast::Literal::Int(4)
            ))])
        );
    }

    #[test]
    fn string_expression_should_work() {
        let input = r#""abc""#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Literal(
                ast::Literal::String("abc".to_string())
            ))])
        );
    }

    #[test]
    fn boolean_expression_should_work() {
        let input = "false;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Literal(
                ast::Literal::Bool(false)
            ))])
        );
    }

    #[test]
    fn minus_expression_should_work() {
        let input = "-5;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Prefix(
                ast::Prefix::Minus,
                Box::new(ast::Expression::Literal(ast::Literal::Int(5)))
            ))])
        );
    }

    #[test]
    fn bang_expression_should_work() {
        let input = "!true;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Prefix(
                ast::Prefix::Not,
                Box::new(ast::Expression::Literal(ast::Literal::Bool(true)))
            ))])
        );
    }

    #[test]
    fn function_should_work() {
        let input = r#"
        fn() {1};
        fn(x) {x};
        fn(x,y) {x+y};
        "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![
                ast::Statement::Expression(ast::Expression::Func {
                    params: Vec::new(),
                    body: ast::BlockStatement(vec![ast::Statement::Expression(
                        ast::Expression::Literal(ast::Literal::Int(1))
                    )]),
                }),
                ast::Statement::Expression(ast::Expression::Func {
                    params: vec![ast::Ident("x".to_string())],
                    body: ast::BlockStatement(vec![ast::Statement::Expression(
                        ast::Expression::Ident(ast::Ident("x".to_string()))
                    )]),
                }),
                ast::Statement::Expression(ast::Expression::Func {
                    params: vec![ast::Ident("x".to_string()), ast::Ident("y".to_string())],
                    body: ast::BlockStatement(vec![ast::Statement::Expression(
                        ast::Expression::Infix(
                            ast::Infix::Plus,
                            Box::new(ast::Expression::Ident(ast::Ident("x".to_string()))),
                            Box::new(ast::Expression::Ident(ast::Ident("y".to_string()))),
                        )
                    )]),
                }),
            ])
        )
    }

    #[test]
    fn call_should_work() {
        let input = r#"
        add();
        add(1,2);
        add(1+2, 3+4);
        "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![
                ast::Statement::Expression(ast::Expression::Call {
                    func: Box::new(ast::Expression::Ident(ast::Ident("add".to_string()))),
                    args: Vec::new(),
                }),
                ast::Statement::Expression(ast::Expression::Call {
                    func: Box::new(ast::Expression::Ident(ast::Ident("add".to_string()))),
                    args: vec![
                        ast::Expression::Literal(ast::Literal::Int(1)),
                        ast::Expression::Literal(ast::Literal::Int(2))
                    ],
                }),
                ast::Statement::Expression(ast::Expression::Call {
                    func: Box::new(ast::Expression::Ident(ast::Ident("add".to_string()))),
                    args: vec![
                        ast::Expression::Infix(
                            ast::Infix::Plus,
                            Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                            Box::new(ast::Expression::Literal(ast::Literal::Int(2)))
                        ),
                        ast::Expression::Infix(
                            ast::Infix::Plus,
                            Box::new(ast::Expression::Literal(ast::Literal::Int(3))),
                            Box::new(ast::Expression::Literal(ast::Literal::Int(4)))
                        )
                    ],
                }),
            ])
        )
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

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::If {
                condition: Box::new(ast::Expression::Infix(
                    ast::Infix::Lt,
                    Box::new(ast::Expression::Ident(ast::Ident("x".to_string()))),
                    Box::new(ast::Expression::Ident(ast::Ident("y".to_string()))),
                )),
                consequence: ast::BlockStatement(vec![ast::Statement::Expression(
                    ast::Expression::Ident(ast::Ident("x".to_string()))
                )]),
                alternative: Some(ast::BlockStatement(vec![ast::Statement::Expression(
                    ast::Expression::Ident(ast::Ident("y".to_string()))
                )])),
            })])
        )
    }
}
