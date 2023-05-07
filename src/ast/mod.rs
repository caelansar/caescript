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

macro_rules! impl_statement {
    ($($t:ty),*) => {
        $(
            impl Statement for $t {}
        )*
    };
}

macro_rules! impl_expression {
    ($($t:ty),*) => {
        $(
            impl Expression for $t {}
        )*
    };
}

impl_statement!(
    LetStatement,
    ReturnStatement,
    ExpressionStatement,
    BlockStatement
);
impl_expression!(
    Identifier,
    PrefixExpression,
    InfixExpression,
    Literal<i64>,
    Literal<bool>,
    Literal<String>,
    IfExpression
);

impl<T: 'static> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
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
        let mut out = "let".to_string();
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
        self.token.to_string()
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
        self.token.to_string()
    }
}

pub(crate) struct ReturnStatement {
    token: token::Token,
    value: Option<Box<dyn Expression>>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = "return".to_string();
        out.push_str(" ");

        self.value.as_ref().map(|x| out.push_str(&x.to_string()));
        out.push_str(";");

        f.write_str(&out)
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
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
        self.token.to_string()
    }
}

pub struct Literal<T> {
    pub(crate) token: token::Token,
    pub(crate) value: T,
}

impl<T> Display for Literal<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.token.to_string())
    }
}

impl<T> Node for Literal<T> {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl<T> Literal<T> {
    pub(crate) fn new(token: token::Token, value: T) -> Self {
        Self { token, value }
    }
}

pub struct PrefixExpression {
    pub(crate) token: token::Token,
    pub(crate) operator: String,
    pub(crate) rhs: Box<dyn Expression>,
}

impl PrefixExpression {
    pub(crate) fn new(token: token::Token, operator: String, rhs: Box<dyn Expression>) -> Self {
        Self {
            token,
            operator,
            rhs,
        }
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();

        out.push_str("(");
        out.push_str(&self.operator);
        out.push_str(&self.rhs.to_string());
        out.push_str(")");

        f.write_str(&out)
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

pub struct InfixExpression {
    pub(crate) token: token::Token,
    pub(crate) lhs: Box<dyn Expression>,
    pub(crate) operator: String,
    pub(crate) rhs: Box<dyn Expression>,
}

impl InfixExpression {
    pub(crate) fn new(
        token: token::Token,
        lhs: Box<dyn Expression>,
        operator: String,
        rhs: Box<dyn Expression>,
    ) -> Self {
        Self {
            token,
            lhs,
            operator,
            rhs,
        }
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();

        out.push_str("(");
        out.push_str(&self.lhs.to_string());
        out.push_str(" ");
        out.push_str(&self.operator);
        out.push_str(" ");
        out.push_str(&self.rhs.to_string());
        out.push_str(")");

        f.write_str(&out)
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

pub struct IfExpression {
    pub(crate) token: token::Token,
    pub(crate) condition: Box<dyn Expression>,
    pub(crate) consequence: BlockStatement,
    pub(crate) alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub(crate) fn new(
        token: token::Token,
        condition: Box<dyn Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Self {
        Self {
            token,
            condition,
            consequence,
            alternative,
        }
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();

        out.push_str("if ");
        out.push_str(&self.condition.to_string());
        out.push_str(" ");
        out.push_str(&self.consequence.to_string());
        out.push_str(" ");
        if let Some(ref alternative) = self.alternative {
            out.push_str("else ");
            out.push_str(&alternative.to_string());
        }

        f.write_str(&out)
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

pub struct BlockStatement {
    pub(crate) token: token::Token,
    pub(crate) statements: Vec<Box<dyn Statement>>,
}

impl BlockStatement {
    pub(crate) fn new(token: token::Token, statements: Vec<Box<dyn Statement>>) -> Self {
        Self { token, statements }
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();

        self.statements.iter().for_each(|stmt| {
            out.push_str(&stmt.to_string());
        });

        f.write_str(&out)
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

#[cfg(test)]
mod test {
    use crate::token::Token;

    use super::{Identifier, LetStatement, Program};

    #[test]
    fn node_display_should_work() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token::Let,
                name: Identifier {
                    token: Token::Ident("a".to_string()),
                    value: "a".to_string(),
                },
                value: Some(Box::new(Identifier {
                    token: Token::Ident("b".to_string()),
                    value: "b".to_string(),
                })),
            })],
        };
        assert_eq!("let a = b;", program.to_string())
    }
}
