use crate::ast;

use self::object::{Object, BOOL_OBJ_FALSE, BOOL_OBJ_TRUE};

mod object;

pub fn eval(program: &ast::Program) -> Option<Object> {
    let mut rv = None;

    for stmt in program.iter() {
        match eval_statement(stmt) {
            obj => rv = obj,
        }
    }

    rv
}

fn eval_statement(stmt: &ast::Statement) -> Option<Object> {
    match stmt {
        ast::Statement::Expression(expr) => eval_expression(expr),
        _ => todo!(),
    }
}

fn eval_expression(expr: &ast::Expression) -> Option<Object> {
    match expr {
        ast::Expression::Literal(lit) => eval_literal(lit),
        _ => todo!(),
    }
}

fn eval_literal(literal: &ast::Literal) -> Option<Object> {
    match literal {
        ast::Literal::Int(i) => Some(Object::Int(i.clone())),
        ast::Literal::Bool(b) => match b {
            true => Some(BOOL_OBJ_TRUE),
            false => Some(BOOL_OBJ_FALSE),
        },
        _ => todo!(),
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer, parser::Parser};

    use super::*;

    #[test]
    fn eval_int_should_work() {
        let input = "1";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(Some(Object::Int(1)), eval(&program));
    }

    #[test]
    fn eval_bool_should_work() {
        let input = "false";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(Some(Object::Bool(false)), eval(&program));

        let input = "true";

        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(Some(Object::Bool(true)), eval(&program));
    }
}
