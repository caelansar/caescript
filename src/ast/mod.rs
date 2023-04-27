use std::{any::Any, fmt::Display};

use crate::token;

pub(crate) trait AsAny {
    fn as_any(&self) -> &dyn Any;
}

pub(crate) trait Node: Display {
    fn token_literal(&self) -> String;
}

pub(crate) trait Statement: Node + AsAny {}

pub(crate) trait Expression: Node + AsAny {}

pub(crate) struct Program {
    pub(crate) statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub(crate) fn new(statements: Vec<Box<dyn Statement>>) -> Self {
        Self { statements }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();
        self.statements
            .iter()
            .for_each(|x| out.push_str(&x.to_string()));
        f.write_str(&out)
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".into()
        }
    }
}

pub(crate) struct LetStatement {
    pub(crate) token: token::Token,
    pub(crate) name: Identifier,
    pub(crate) value: Option<Box<dyn Expression>>,
}

impl LetStatement {
    pub(crate) fn new(token: token::Token, name: Identifier) -> Self {
        Self {
            token,
            name,
            value: None,
        }
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = self.token_literal();
        out.push_str(" ");
        out.push_str(&self.name.to_string());
        out.push_str(" = ");

        self.value.as_ref().map(|x| out.push_str(&x.to_string()));
        out.push_str(";");

        f.write_str(&out)
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for LetStatement {}

impl AsAny for LetStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Identifier {
    pub(crate) token: token::Token,
    pub(crate) value: String,
}

impl Identifier {
    pub(crate) fn new(token: token::Token, value: String) -> Self {
        Self { token, value }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.value)
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for Identifier {}

impl AsAny for Identifier {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub(crate) struct ReturnStatement {
    token: token::Token,
    value: Option<Box<dyn Expression>>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = self.token_literal();
        out.push_str(" ");

        self.value.as_ref().map(|x| out.push_str(&x.to_string()));
        out.push_str(";");

        f.write_str(&out)
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for ReturnStatement {}

impl AsAny for ReturnStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ReturnStatement {
    pub(crate) fn new(token: token::Token) -> Self {
        Self { token, value: None }
    }
}

pub(crate) struct ExpressionStatement {
    pub(crate) token: token::Token,
    pub(crate) expression: Option<Box<dyn Expression>>,
}

impl ExpressionStatement {
    pub(crate) fn new(token: token::Token, expression: Option<Box<dyn Expression>>) -> Self {
        Self { token, expression }
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            self.expression
                .as_ref()
                .and_then(|x| Some(x.to_string()))
                .or_else(|| Some("".to_string()))
                .unwrap()
                .as_str(),
        )
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for ExpressionStatement {}

impl AsAny for ExpressionStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[cfg(test)]
mod test {
    use crate::token::{Token, TokenType};

    use super::{Identifier, LetStatement, Program};

    #[test]
    fn node_display_should_work() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token::new(TokenType::Let, "let".to_string()),
                name: Identifier {
                    token: Token::new(TokenType::Let, "a".to_string()),
                    value: "a".to_string(),
                },
                value: Some(Box::new(Identifier {
                    token: Token::new(TokenType::Let, "b".to_string()),
                    value: "b".to_string(),
                })),
            })],
        };
        assert_eq!("let a = b;", program.to_string())
    }
}
