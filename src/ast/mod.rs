use std::fmt::{self, Display};

use crate::token;

#[derive(PartialEq, Clone, Debug)]
pub struct Ident(pub String);

#[derive(PartialEq, Clone, Debug)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Prefix::Plus => write!(f, "+"),
            Prefix::Minus => write!(f, "-"),
            Prefix::Not => write!(f, "!"),
        }
    }
}

impl TryFrom<token::Token> for Prefix {
    type Error = &'static str;

    fn try_from(value: token::Token) -> Result<Self, Self::Error> {
        match value {
            token::Token::Bang => Ok(Prefix::Not),
            token::Token::Minus => Ok(Prefix::Minus),
            token::Token::Plus => Ok(Prefix::Plus),
            _ => Err("invalid token"),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Eq,
    Ne,
    Gt,
    Lt,
}

impl TryFrom<token::Token> for Infix {
    type Error = &'static str;

    fn try_from(value: token::Token) -> Result<Self, Self::Error> {
        match value {
            token::Token::Plus => Ok(Infix::Plus),
            token::Token::Minus => Ok(Infix::Minus),
            token::Token::Slash => Ok(Infix::Divide),
            token::Token::Asterisk => Ok(Infix::Multiply),
            token::Token::Eq => Ok(Infix::Eq),
            token::Token::Ne => Ok(Infix::Ne),
            token::Token::Lt => Ok(Infix::Lt),
            token::Token::Gt => Ok(Infix::Gt),
            _ => Err("invalid token"),
        }
    }
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Divide => write!(f, "/"),
            Infix::Multiply => write!(f, "*"),
            Infix::Eq => write!(f, "=="),
            Infix::Ne => write!(f, "!="),
            Infix::Gt => write!(f, ">"),
            Infix::Lt => write!(f, "<"),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    If {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Ident(Ident(ident)) => f.write_str(&ident),
            Expression::Literal(ref literal) => f.write_str(&literal.to_string()),
            Expression::Prefix(prefix, expr) => {
                let mut out = String::new();

                out.push_str("(");
                out.push_str(&prefix.to_string());
                out.push_str(&expr.to_string());
                out.push_str(")");

                f.write_str(&out)
            }
            Expression::Infix(infix, lexpr, rexpr) => {
                let mut out = String::new();

                out.push_str("(");
                out.push_str(&lexpr.to_string());
                out.push_str(" ");
                out.push_str(&infix.to_string());
                out.push_str(" ");
                out.push_str(&rexpr.to_string());
                out.push_str(")");

                f.write_str(&out)
            }
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                let mut out = String::new();

                out.push_str("if ");
                out.push_str(&condition.to_string());
                out.push_str(" ");
                out.push_str(&consequence.to_string());
                out.push_str(" ");
                if let Some(ref alternative) = alternative {
                    out.push_str("else ");
                    out.push_str(&alternative.to_string());
                }

                f.write_str(&out)
            }
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        match self {
            Literal::Int(int) => out.push_str(&int.to_string()),
            Literal::String(string) => out.push_str(&string),
            Literal::Bool(b) => out.push_str(&b.to_string()),
        }
        f.write_str(&out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Let(Ident, Expression),
    Return(Expression),
    Expression(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(Ident(ident), expr) => {
                let mut out = "let".to_string();
                out.push_str(" ");
                out.push_str(&ident.to_string());
                out.push_str(" = ");

                out.push_str(&expr.to_string());
                out.push_str(";");

                f.write_str(&out)
            }
            Statement::Return(expr) => {
                let mut out = "return".to_string();
                out.push_str(" ");

                out.push_str(&expr.to_string());
                out.push_str(";");

                f.write_str(&out)
            }
            Statement::Expression(expr) => f.write_str(&expr.to_string()),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct BlockStatement(pub Vec<Statement>);

impl Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();

        self.0.iter().for_each(|stmt| {
            out.push_str(&stmt.to_string());
        });

        f.write_str(&out)
    }
}

pub type Program = BlockStatement;

#[cfg(test)]
mod test {
    use super::{BlockStatement, Expression, Ident, Infix, Prefix, Program, Statement};

    #[test]
    fn display_should_work() {
        let program: Program = BlockStatement(vec![Statement::Let(
            super::Ident("a".to_string()),
            Expression::Ident(Ident("b".to_string())),
        )]);
        assert_eq!("let a = b;", program.to_string());

        let program: Program = BlockStatement(vec![Statement::Expression(Expression::Infix(
            Infix::Plus,
            Box::new(Expression::Ident(Ident("a".to_string()))),
            Box::new(Expression::Ident(Ident("b".to_string()))),
        ))]);
        assert_eq!("(a + b)", program.to_string());

        let program: Program = BlockStatement(vec![Statement::Expression(Expression::Prefix(
            Prefix::Minus,
            Box::new(Expression::Ident(Ident("a".to_string()))),
        ))]);
        assert_eq!("(-a)", program.to_string())
    }
}
