use std::{
    fmt::{self, Display},
    ops::Deref,
};

use crate::token;

#[derive(PartialEq, Clone, Debug)]
pub struct Ident(pub String);

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Prefix {
    Minus,
    Not,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Prefix::Minus => write!(f, "-"),
            Prefix::Not => write!(f, "!"),
        }
    }
}

impl TryFrom<&token::Token> for Prefix {
    type Error = &'static str;

    fn try_from(value: &token::Token) -> Result<Self, Self::Error> {
        match value {
            token::Token::Bang => Ok(Prefix::Not),
            token::Token::Minus => Ok(Prefix::Minus),
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
    Mod,
    Eq,
    Ne,
    Gt,
    Lt,
    GtEq,
    LtEq,
    And,
    Or,
}

impl TryFrom<&token::Token> for Infix {
    type Error = &'static str;

    fn try_from(value: &token::Token) -> Result<Self, Self::Error> {
        match value {
            token::Token::Plus => Ok(Infix::Plus),
            token::Token::Minus => Ok(Infix::Minus),
            token::Token::Slash => Ok(Infix::Divide),
            token::Token::Asterisk => Ok(Infix::Multiply),
            token::Token::Mod => Ok(Infix::Mod),
            token::Token::Eq => Ok(Infix::Eq),
            token::Token::Ne => Ok(Infix::Ne),
            token::Token::Lt => Ok(Infix::Lt),
            token::Token::Gt => Ok(Infix::Gt),
            token::Token::GtEq => Ok(Infix::GtEq),
            token::Token::LtEq => Ok(Infix::LtEq),
            token::Token::And => Ok(Infix::And),
            token::Token::Or => Ok(Infix::Or),
            _ => Err("invalid infix token"),
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
            Infix::Mod => write!(f, "%"),
            Infix::Eq => write!(f, "=="),
            Infix::Ne => write!(f, "!="),
            Infix::Gt => write!(f, ">"),
            Infix::GtEq => write!(f, ">="),
            Infix::Lt => write!(f, "<"),
            Infix::LtEq => write!(f, "<="),
            Infix::And => write!(f, "&&"),
            Infix::Or => write!(f, "||"),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Assign {
    Assign,
    PlusEq,
    MinusEq,
    DivideEq,
    MultiplyEq,
    ModEq,
}

impl TryFrom<&token::Token> for Assign {
    type Error = &'static str;

    fn try_from(value: &token::Token) -> Result<Self, Self::Error> {
        match value {
            token::Token::Assign => Ok(Assign::Assign),
            token::Token::PlusEq => Ok(Assign::PlusEq),
            token::Token::MinusEq => Ok(Assign::MinusEq),
            token::Token::SlashEq => Ok(Assign::DivideEq),
            token::Token::ModEq => Ok(Assign::ModEq),
            token::Token::AsteriskEq => Ok(Assign::MultiplyEq),
            _ => Err("invalid token"),
        }
    }
}

impl fmt::Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Assign::Assign => write!(f, "="),
            Assign::PlusEq => write!(f, "+="),
            Assign::MinusEq => write!(f, "-="),
            Assign::DivideEq => write!(f, "/="),
            Assign::MultiplyEq => write!(f, "*="),
            Assign::ModEq => write!(f, "%="),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    For {
        condition: Box<Expression>,
        consequence: BlockStatement,
    },
    If {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Func {
        name: Option<String>,
        params: Vec<Ident>,
        body: BlockStatement,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    Assign(Assign, Ident, Box<Expression>),
    Array(Vec<Expression>),
    Hash(Vec<(Expression, Expression)>),
    Index(Box<Expression>, Box<Expression>),
    Null,
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Ident(Ident(ident)) => f.write_str(ident),
            Expression::Literal(ref literal) => f.write_str(&literal.to_string()),
            Expression::Prefix(prefix, expr) => {
                let mut out = String::new();

                out.push('(');
                out.push_str(&prefix.to_string());
                out.push_str(&expr.to_string());
                out.push(')');

                f.write_str(&out)
            }
            Expression::Infix(infix, lexpr, rexpr) => {
                let mut out = String::new();

                out.push('(');
                out.push_str(&lexpr.to_string());
                out.push(' ');
                out.push_str(&infix.to_string());
                out.push(' ');
                out.push_str(&rexpr.to_string());
                out.push(')');

                f.write_str(&out)
            }
            Expression::For {
                condition,
                consequence,
            } => {
                let mut out = String::new();

                out.push_str("if ");
                out.push_str(&condition.to_string());
                out.push(' ');
                out.push_str(&consequence.to_string());
                out.push(' ');
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
                out.push(' ');
                out.push_str(&consequence.to_string());
                out.push(' ');
                if let Some(ref alternative) = alternative {
                    out.push_str("else ");
                    out.push_str(&alternative.to_string());
                }

                f.write_str(&out)
            }
            Expression::Func { name, params, body } => {
                let mut out = String::new();

                out.push_str(&format!("fn {}", name.as_ref().unwrap_or(&"".to_string())));
                out.push('(');
                out.push_str(
                    params
                        .iter()
                        .map(|ident| ident.0.as_str())
                        .collect::<Vec<&str>>()
                        .join(",")
                        .as_str(),
                );
                out.push(')');

                out.push('{');
                out.push_str(&body.to_string());
                out.push('}');

                f.write_str(&out)
            }
            Expression::Call { func, args } => {
                let mut out = String::new();

                out.push_str(&func.to_string());
                out.push('(');
                out.push_str(
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
                        .as_str(),
                );
                out.push(')');

                f.write_str(&out)
            }
            Expression::Assign(op, Ident(ident), expr) => {
                let mut out = "".to_string();
                out.push_str(&ident.to_string());
                out.push_str(&format!(" {} ", op));

                out.push_str(&expr.to_string());
                out.push(';');

                f.write_str(&out)
            }
            Expression::Array(elements) => f.write_str(&format!(
                "[{}]",
                elements
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )),
            Expression::Hash(hash) => f.write_str(&format!(
                "{{{}}}",
                hash.iter()
                    .map(|(k, v)| format!("{}: {}", k.to_string(), v.to_string()))
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
            Expression::Index(lhs, idx) => {
                f.write_str(&format!("{}[{}]", lhs.to_string(), idx.to_string()))
            }
            Expression::Null => f.write_str("null"),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        match self {
            Literal::Int(int) => out.push_str(&int.to_string()),
            Literal::Float(float) => out.push_str(&float.to_string()),
            Literal::String(string) => out.push_str(string),
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
    Break,
    Continue,
    Function(Ident, Vec<Ident>, BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(Ident(ident), expr) => {
                let mut out = "let".to_string();
                out.push(' ');
                out.push_str(&ident.to_string());
                out.push_str(" = ");

                out.push_str(&expr.to_string());
                out.push(';');

                f.write_str(&out)
            }
            Statement::Return(expr) => {
                let mut out = "return".to_string();
                out.push(' ');

                out.push_str(&expr.to_string());
                out.push(';');

                f.write_str(&out)
            }
            Statement::Function(ident, params, body) => {
                let mut out = String::new();

                out.push_str("fn ");
                out.push_str(&ident.0);
                out.push('(');
                out.push_str(
                    params
                        .iter()
                        .map(|ident| ident.0.as_str())
                        .collect::<Vec<&str>>()
                        .join(",")
                        .as_str(),
                );
                out.push(')');

                out.push('{');
                out.push_str(&body.to_string());
                out.push('}');

                f.write_str(&out)
            }
            Statement::Expression(expr) => f.write_str(&expr.to_string()),
            Statement::Break => f.write_str("break"),
            Statement::Continue => f.write_str("continue"),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct BlockStatement(pub Vec<Statement>);

impl Deref for BlockStatement {
    type Target = Vec<Statement>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

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
