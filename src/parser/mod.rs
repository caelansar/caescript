#[cfg(feature = "trace")]
mod trace;

use std::collections::HashMap;

#[cfg(feature = "trace")]
use crate::defer;
use crate::{ast, lexer, map, token};
#[cfg(feature = "trace")]
use trace::{trace, untrace, ScopeCall};

type PrefixParseFn = for<'a> fn(&mut Parser<'a>) -> Option<ast::Expression>;
type InfixParseFn = for<'a> fn(&mut Parser<'a>, Option<ast::Expression>) -> Option<ast::Expression>;

#[derive(PartialEq, PartialOrd, Debug)]
pub(crate) enum Precedence {
    Lowest,
    Assign,
    Logical,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
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
            token::Token::ModEq => Precedence::Assign,
            token::Token::RightShiftEq => Precedence::Assign,
            token::Token::LeftShiftEq => Precedence::Assign,
            token::Token::BitOrEq => Precedence::Assign,
            token::Token::BitXorEq => Precedence::Assign,
            token::Token::BitAndEq => Precedence::Assign,
            token::Token::Ne => Precedence::Equals,
            token::Token::Lt => Precedence::LessGreater,
            token::Token::Gt => Precedence::LessGreater,
            token::Token::LtEq => Precedence::LessGreater,
            token::Token::GtEq => Precedence::LessGreater,
            token::Token::Plus => Precedence::Sum,
            token::Token::Minus => Precedence::Sum,
            token::Token::Slash => Precedence::Product,
            token::Token::Asterisk => Precedence::Product,
            token::Token::Mod => Precedence::Product,
            token::Token::Lparen => Precedence::Call,
            token::Token::Lbracket => Precedence::Index,
            token::Token::And => Precedence::Logical,
            token::Token::Or => Precedence::Logical,
            token::Token::LeftShift => Precedence::Logical,
            token::Token::RightShift => Precedence::Logical,
            token::Token::BitAnd => Precedence::Logical,
            token::Token::BitOr => Precedence::Logical,
            token::Token::BitXor => Precedence::Logical,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    current_token: token::Token,
    next_token: token::Token,
    errors: Vec<String>,
    prefix_parse_map: Option<HashMap<token::Token, PrefixParseFn>>,
    infix_parse_map: Option<HashMap<token::Token, InfixParseFn>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: lexer::Lexer<'a>) -> Self {
        let mut parser = Self {
            lexer,
            current_token: token::Token::Eof,
            next_token: token::Token::Eof,
            errors: Vec::new(),
            prefix_parse_map: None,
            infix_parse_map: None,
        };

        parser.register_parse_fn();

        parser.next_token();
        parser.next_token();
        parser
    }

    fn register_parse_fn(&mut self) {
        let parse_ident: PrefixParseFn = |this| this.parse_identifier();
        let parse_integer: PrefixParseFn = |this| this.parse_integer_literal();
        let parse_float: PrefixParseFn = |this| this.parse_float_literal();
        let parse_bool: PrefixParseFn = |this| this.parse_boolean_literal();
        let parse_string: PrefixParseFn = |this| this.parse_string_literal();
        let parse_prefix: PrefixParseFn = |this| this.parse_prefix_expression();
        let parse_lparen: PrefixParseFn = |this| this.parse_grouped_expression();
        let parse_if: PrefixParseFn = |this| this.parse_if_expression();
        let parse_for: PrefixParseFn = |this| this.parse_for_expression();
        let parse_func: PrefixParseFn = |this| this.parse_function_literal();
        let parse_lbracket: PrefixParseFn = |this| this.parse_array();
        let parse_lbrace: PrefixParseFn = |this| this.parse_hash();

        self.prefix_parse_map = Some(map! {
            token::Token::Ident(Default::default()) => parse_ident,
            token::Token::Int(Default::default())=> parse_integer,
            token::Token::Float(Default::default())=> parse_float,
            token::Token::Bool(Default::default())=> parse_bool,
            token::Token::String(Default::default())=> parse_string,
            token::Token::Minus=> parse_prefix,
            token::Token::Bang=> parse_prefix,
            token::Token::Lparen=> parse_lparen,
            token::Token::If=> parse_if,
            token::Token::For=> parse_for,
            token::Token::Function=> parse_func,
            token::Token::Lbracket=> parse_lbracket,
            token::Token::Lbrace=> parse_lbrace,
            token::Token::Null=> |_| Some(ast::Expression::Null),
        });

        let parse_call: InfixParseFn = |this, lhs| this.parse_call_expression(lhs);
        let parse_infix: InfixParseFn = |this, lhs| this.parse_infix_expression(lhs);
        let parse_assign: InfixParseFn = |this, lhs| this.parse_assign_expression(lhs);
        let parse_index: InfixParseFn = |this, lhs| this.parse_index_expression(lhs);

        self.infix_parse_map = Some(map! {
            token::Token::Plus => parse_infix,
            token::Token::Plus => parse_infix,
            token::Token::Minus => parse_infix,
            token::Token::Eq=> parse_infix,
            token::Token::Plus => parse_infix,
            token::Token::Minus=> parse_infix,
            token::Token::Eq=> parse_infix,
            token::Token::Ne=> parse_infix,
            token::Token::Plus=> parse_infix,
            token::Token::Minus=> parse_infix,
            token::Token::Eq=> parse_infix,
            token::Token::Ne=> parse_infix,
            token::Token::Gt=> parse_infix,
            token::Token::GtEq=> parse_infix,
            token::Token::Lt=> parse_infix,
            token::Token::LtEq=> parse_infix,
            token::Token::Slash=> parse_infix,
            token::Token::Asterisk=> parse_infix,
            token::Token::Mod=> parse_infix,
            token::Token::And=> parse_infix,
            token::Token::Or=> parse_infix,
            token::Token::LeftShift=> parse_infix,
            token::Token::RightShift=> parse_infix,
            token::Token::Plus=> parse_infix,
            token::Token::Minus=> parse_infix,
            token::Token::Eq=> parse_infix,
            token::Token::Ne=> parse_infix,
            token::Token::Gt=> parse_infix,
            token::Token::GtEq=> parse_infix,
            token::Token::Lt=> parse_infix,
            token::Token::LtEq=> parse_infix,
            token::Token::Slash=> parse_infix,
            token::Token::Asterisk=> parse_infix,
            token::Token::Mod=> parse_infix,
            token::Token::And=> parse_infix,
            token::Token::Or=> parse_infix,
            token::Token::LeftShift=> parse_infix,
            token::Token::RightShift=> parse_infix,
            token::Token::BitAnd=> parse_infix,
            token::Token::BitOr=> parse_infix,
            token::Token::BitXor=> parse_infix,
            token::Token::Lparen=> parse_call,
            token::Token::Assign=> parse_assign,
            token::Token::PlusEq=> parse_assign,
            token::Token::MinusEq=> parse_assign,
            token::Token::AsteriskEq=> parse_assign,
            token::Token::SlashEq=> parse_assign,
            token::Token::ModEq=> parse_assign,
            token::Token::Lbracket=> parse_index,
            token::Token::BitOrEq=> parse_assign,
            token::Token::BitAndEq=> parse_assign,
            token::Token::BitXorEq=> parse_assign,
            token::Token::LeftShiftEq=> parse_assign,
            token::Token::RightShiftEq=> parse_assign,
        });
    }

    #[inline(always)]
    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    #[inline(always)]
    fn next_error(&mut self, token: &token::Token) {
        self.errors.push(format!(
            "expect next_token to be {}, got {} instead",
            token, self.next_token
        ))
    }

    fn is_illegal_token(&mut self) -> bool {
        if let token::Token::Illegal(e) = &self.current_token {
            self.errors.push(e.to_string());
            true
        } else {
            false
        }
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
    fn parse_block_statement(&mut self) -> ast::BlockStatement {
        let mut statements = Vec::new();

        self.next_token();

        while !self.current_token_is(&token::Token::Rbrace) {
            if self.current_token_is(&token::Token::Eof) {
                panic!(
                    "unterminated block statement, current token {:?}",
                    self.current_token
                );
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
        defer!(untrace(trace("parse_if_expression")));

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

        let consequence = self.parse_block_statement();

        let mut alternative = None;

        if self.next_token_is(&token::Token::Else) {
            self.next_token();
            if self.expect_next(&token::Token::Lbrace) {
                alternative = Some(self.parse_block_statement());
            }
        }

        Some(ast::Expression::If {
            condition: Box::new(condition.unwrap()),
            consequence,
            alternative,
        })
    }

    #[inline]
    fn parse_for_expression(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace(trace("parse_for_expression")));

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

        let consequence = self.parse_block_statement();

        Some(ast::Expression::For {
            condition: Box::new(condition.unwrap()),
            consequence,
        })
    }

    #[inline(always)]
    fn parse_infix_expression(&mut self, lhs: Option<ast::Expression>) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace(trace("parse_infix_expression")));
        self.next_token();

        let infix: ast::Infix = match (&self.current_token).try_into() {
            Ok(infix) => infix,
            Err(_) => return None,
        };

        lhs.as_ref()?;

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
        defer!(untrace(trace("parse_grouped_expression")));
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
        defer!(untrace(trace("parse_prefix_expression")));
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
        defer!(untrace(trace("parse_identifier")));
        if let token::Token::Ident(ref ident) = self.current_token {
            Some(ast::Expression::Ident(ast::Ident(ident.clone())))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_string_literal(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace(trace("parse_string_literal")));
        if let token::Token::String(ref s) = self.current_token {
            Some(ast::Expression::Literal(ast::Literal::String(s.clone())))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_integer_literal(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace(trace("parse_integer_literal")));
        if let token::Token::Int(int) = self.current_token {
            Some(ast::Expression::Literal(ast::Literal::Int(int)))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_float_literal(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace(trace("parse_integer_literal")));
        if let token::Token::Float(float) = self.current_token {
            Some(ast::Expression::Literal(ast::Literal::Float(float)))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_boolean_literal(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace(trace("parse_boolean_literal")));
        if let token::Token::Bool(b) = self.current_token {
            Some(ast::Expression::Literal(ast::Literal::Bool(b)))
        } else {
            None
        }
    }

    fn parse_array(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace(trace("parse_array")));

        let mut exprs = vec![];

        if self.next_token_is(&token::Token::Rbracket) {
            self.next_token();
            return Some(ast::Expression::Array(exprs));
        }

        self.next_token();

        exprs.push(self.parse_expression(Precedence::Lowest)?);

        while self.next_token_is(&token::Token::Comma) {
            self.next_token();
            self.next_token();

            exprs.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_next(&token::Token::Rbracket) {
            return None;
        }

        Some(ast::Expression::Array(exprs))
    }

    fn parse_hash(&mut self) -> Option<ast::Expression> {
        let mut hash = vec![];

        while !self.next_token_is(&token::Token::Rbrace)
            && !self.next_token_is(&token::Token::Eof)
            && !self.next_token_is(&token::Token::SemiColon)
        {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest)?;

            if !self.expect_next(&token::Token::Colon) {
                return None;
            }

            self.next_token();

            let value = self.parse_expression(Precedence::Lowest)?;

            // if next token is Comma, should move forward
            if !self.next_token_is(&token::Token::Rbrace) && !self.expect_next(&token::Token::Comma)
            {
                return None;
            }

            hash.push((key, value));
        }

        if !self.expect_next(&token::Token::Rbrace) {
            return None;
        }
        Some(ast::Expression::Hash(hash))
    }

    #[inline(always)]
    fn parse_function_literal(&mut self) -> Option<ast::Expression> {
        #[cfg(feature = "trace")]
        defer!(untrace(trace("parse_function_literal")));
        if !self.expect_next(&token::Token::Lparen) {
            return None;
        }

        let params = self.parse_function_params()?;

        if !self.expect_next(&token::Token::Lbrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(ast::Expression::Func {
            name: None,
            params,
            body,
        })
    }

    #[inline]
    fn parse_function_statement(&mut self) -> Option<ast::Statement> {
        if let token::Token::Ident(ident) = self.next_token.clone() {
            self.next_token();
            if !self.expect_next(&token::Token::Lparen) {
                return None;
            }
            let params = self.parse_function_params()?;

            if !self.expect_next(&token::Token::Lbrace) {
                return None;
            }

            let body = self.parse_block_statement();

            if self.next_token_is(&token::Token::SemiColon) {
                self.next_token();
            }

            Some(ast::Statement::Function(ast::Ident(ident), params, body))
        } else {
            self.parse_expression_statement()
        }
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

    fn parse_assign_expression(&mut self, lhs: Option<ast::Expression>) -> Option<ast::Expression> {
        let op: ast::Assign = (&self.next_token).try_into().unwrap();
        if let Some(ast::Expression::Ident(ident)) = lhs {
            self.next_token();
            self.next_token();

            let expr = self.parse_expression(Precedence::Lowest)?;

            if self.next_token_is(&token::Token::SemiColon) {
                self.next_token();
            }
            Some(ast::Expression::Assign(op, ident, Box::new(expr)))
        } else {
            None
        }
    }

    #[inline(always)]
    fn parse_call_expression(&mut self, lhs: Option<ast::Expression>) -> Option<ast::Expression> {
        self.next_token();

        lhs.as_ref()?;

        let args = self.parse_call_args()?;

        Some(ast::Expression::Call {
            func: Box::new(lhs.unwrap()),
            args,
        })
    }

    #[inline(always)]
    fn parse_index_expression(&mut self, lhs: Option<ast::Expression>) -> Option<ast::Expression> {
        lhs.as_ref()?;

        self.next_token();
        self.next_token();

        let idx = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_next(&token::Token::Rbracket) {
            return None;
        }

        Some(ast::Expression::Index(
            Box::new(lhs.unwrap()),
            Box::new(idx),
        ))
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

    pub fn parse_program(&mut self) -> Result<ast::Program, String> {
        let mut stmts = Vec::new();

        while self.current_token != token::Token::Eof {
            if self.is_illegal_token() {
                return Err(self.errors().join("\n"));
            }
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                stmts.push(stmt);
            }
            self.next_token();
        }

        let errors = self.errors();

        if !errors.is_empty() {
            let msg = errors.join("\n");

            return Err(msg);
        }

        Ok(ast::BlockStatement(stmts))
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.current_token {
            token::Token::Let => self.parse_let_statement(),
            token::Token::Return => self.parse_return_statement(),
            token::Token::Function => self.parse_function_statement(),
            token::Token::Break => {
                if self.next_token_is(&token::Token::SemiColon) {
                    self.next_token();
                }
                Some(ast::Statement::Break)
            }
            token::Token::Continue => {
                if self.next_token_is(&token::Token::SemiColon) {
                    self.next_token();
                }
                Some(ast::Statement::Continue)
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.next_token_is(&token::Token::SemiColon) {
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

        let mut expr = self.parse_expression(Precedence::Lowest)?;
        if let ast::Expression::Func {
            name: _,
            params,
            body,
        } = expr
        {
            expr = ast::Expression::Func {
                name: Some(identifier.0.clone()),
                params,
                body,
            }
        }

        if self.next_token_is(&token::Token::SemiColon) {
            self.next_token();
        }
        Some(ast::Statement::Let(identifier, expr))
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        #[cfg(feature = "trace")]
        defer!(untrace(trace("parse_expression_statement")));
        let expression = self.parse_expression(Precedence::Lowest)?;
        let stmt = ast::Statement::Expression(expression);

        // eat SemiColon
        if self.next_token_is(&token::Token::SemiColon) {
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
        defer!(untrace(trace("parse_expression")));

        let mut lhs = match self
            .prefix_parse_map
            .as_ref()
            .unwrap()
            .get(&self.current_token)
        {
            Some(parse_fn) => parse_fn(self),
            None => {
                self.no_prefix_parse_fn_error(&self.current_token.clone());
                return None;
            }
        };

        while !self.next_token_is(&token::Token::SemiColon) && precedence < self.next_precedence() {
            match self.infix_parse_map.as_ref().unwrap().get(&self.next_token) {
                Some(parse_fn) => lhs = parse_fn(self, lhs),
                None => return lhs,
            };
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

        assert!(errs.is_empty())
    }

    #[test]
    #[should_panic]
    fn illegal_float_should_failed() {
        let input = "1.2.3.";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse_program().unwrap();
    }

    #[test]
    fn assign_statement_should_work() {
        let input = r#"
            a = 1;
            b = 1 + 2;
            a += 1;
            "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![
                ast::Statement::Expression(ast::Expression::Assign(
                    ast::Assign::Assign,
                    ast::Ident("a".to_string()),
                    Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                )),
                ast::Statement::Expression(ast::Expression::Assign(
                    ast::Assign::Assign,
                    ast::Ident("b".to_string()),
                    Box::new(ast::Expression::Infix(
                        ast::Infix::Plus,
                        Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                        Box::new(ast::Expression::Literal(ast::Literal::Int(2))),
                    )),
                )),
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
            let e = f
            "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
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
                ast::Statement::Let(
                    ast::Ident("e".to_string()),
                    ast::Expression::Ident(ast::Ident("f".to_string())),
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

        let program = parser.parse_program().unwrap();
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

        let program = parser.parse_program().unwrap();
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

        let program = parser.parse_program().unwrap();
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

        let program = parser.parse_program().unwrap();
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

        let program = parser.parse_program().unwrap();
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

        let program = parser.parse_program().unwrap();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Prefix(
                ast::Prefix::Minus,
                Box::new(ast::Expression::Literal(ast::Literal::Int(5))),
            ))])
        );
    }

    #[test]
    fn bang_expression_should_work() {
        let input = "!true;";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Prefix(
                ast::Prefix::Not,
                Box::new(ast::Expression::Literal(ast::Literal::Bool(true))),
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

        let program = parser.parse_program().unwrap();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![
                ast::Statement::Expression(ast::Expression::Func {
                    name: None,
                    params: Vec::new(),
                    body: ast::BlockStatement(vec![ast::Statement::Expression(
                        ast::Expression::Literal(ast::Literal::Int(1))
                    )]),
                }),
                ast::Statement::Expression(ast::Expression::Func {
                    name: None,
                    params: vec![ast::Ident("x".to_string())],
                    body: ast::BlockStatement(vec![ast::Statement::Expression(
                        ast::Expression::Ident(ast::Ident("x".to_string()))
                    )]),
                }),
                ast::Statement::Expression(ast::Expression::Func {
                    name: None,
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

        let program = parser.parse_program().unwrap();
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
                        ast::Expression::Literal(ast::Literal::Int(2)),
                    ],
                }),
                ast::Statement::Expression(ast::Expression::Call {
                    func: Box::new(ast::Expression::Ident(ast::Ident("add".to_string()))),
                    args: vec![
                        ast::Expression::Infix(
                            ast::Infix::Plus,
                            Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                            Box::new(ast::Expression::Literal(ast::Literal::Int(2))),
                        ),
                        ast::Expression::Infix(
                            ast::Infix::Plus,
                            Box::new(ast::Expression::Literal(ast::Literal::Int(3))),
                            Box::new(ast::Expression::Literal(ast::Literal::Int(4))),
                        ),
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

            let program = parser.parse_program().unwrap();
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

        let program = parser.parse_program().unwrap();
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

    #[test]
    fn for_expression_should_work() {
        let input = "for (true) {x}";
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::For {
                condition: Box::new(ast::Expression::Literal(ast::Literal::Bool(true))),
                consequence: ast::BlockStatement(vec![ast::Statement::Expression(
                    ast::Expression::Ident(ast::Ident("x".to_string()))
                )]),
            })])
        )
    }

    #[test]
    fn array_should_work() {
        let input = r#"
        [1,2,3];
        "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Array(
                vec![
                    ast::Expression::Literal(ast::Literal::Int(1)),
                    ast::Expression::Literal(ast::Literal::Int(2)),
                    ast::Expression::Literal(ast::Literal::Int(3)),
                ]
            ))])
        )
    }

    #[test]
    fn index_should_work() {
        let input = r#"
        [1,2,3][1];
        "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Index(
                Box::new(ast::Expression::Array(vec![
                    ast::Expression::Literal(ast::Literal::Int(1)),
                    ast::Expression::Literal(ast::Literal::Int(2)),
                    ast::Expression::Literal(ast::Literal::Int(3)),
                ])),
                Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
            ))])
        )
    }

    #[test]
    fn hash_should_work() {
        let input = r#"
        {1:2, "a":"b", true: false};
        "#;
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap();
        check_parse_error(&parser);

        assert_eq!(
            program,
            ast::BlockStatement(vec![ast::Statement::Expression(ast::Expression::Hash(
                vec![
                    (
                        ast::Expression::Literal(ast::Literal::Int(1)),
                        (ast::Expression::Literal(ast::Literal::Int(2)))
                    ),
                    (
                        ast::Expression::Literal(ast::Literal::String("a".to_string())),
                        (ast::Expression::Literal(ast::Literal::String("b".to_string())))
                    ),
                    (
                        ast::Expression::Literal(ast::Literal::Bool(true)),
                        (ast::Expression::Literal(ast::Literal::Bool(false)))
                    ),
                ]
            ))])
        )
    }
}
