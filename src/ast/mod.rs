use crate::token;

pub(crate) trait Node {
    fn token_literal(&self) -> String;
    fn name(&self) -> Option<Identifier> {
        None
    }
}

pub(crate) trait Statement: Node {}

pub(crate) trait Expression: Node {}

pub(crate) struct Program {
    pub(crate) statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub(crate) fn new(statements: Vec<Box<dyn Statement>>) -> Self {
        Self { statements }
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

    fn name(&self) -> Option<Identifier> {
        if self.statements.len() > 0 {
            self.statements[0].name()
        } else {
            None
        }
    }
}

pub(crate) struct LetStatement {
    token: token::Token,
    name: Identifier,
    value: Option<Box<dyn Expression>>,
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

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn name(&self) -> Option<Identifier> {
        Some(self.name.clone())
    }
}

impl Statement for LetStatement {}

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

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for Identifier {}

pub(crate) struct ReturnStatement {
    token: token::Token,
    value: Option<Box<dyn Expression>>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for ReturnStatement {}

impl ReturnStatement {
    pub(crate) fn new(token: token::Token) -> Self {
        Self { token, value: None }
    }
}
