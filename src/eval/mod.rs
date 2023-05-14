use crate::ast;

use self::object::*;

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
        _ => Some(Object::Null),
    }
}

fn eval_expression(expr: &ast::Expression) -> Option<Object> {
    match expr {
        ast::Expression::Literal(lit) => eval_literal(lit),
        ast::Expression::Prefix(prefix, rhs) => {
            if let Some(obj) = eval_expression(rhs) {
                eval_prefix_expression(prefix, obj)
            } else {
                None
            }
        }
        _ => Some(Object::Null),
    }
}

fn eval_prefix_expression(prefix: &ast::Prefix, obj: Object) -> Option<Object> {
    match prefix {
        ast::Prefix::Minus => eval_minus_prefix(obj),
        ast::Prefix::Not => eval_not_prefix(obj),
    }
}

fn eval_not_prefix(obj: Object) -> Option<Object> {
    match obj {
        Object::Bool(b) => Some((!b).into()),
        Object::Null => Some(BOOL_OBJ_TRUE),
        _ => Some(BOOL_OBJ_FALSE),
    }
}

fn eval_minus_prefix(obj: Object) -> Option<Object> {
    if let Object::Int(int) = obj {
        Some(Object::Int(-int))
    } else {
        None
    }
}

fn eval_literal(literal: &ast::Literal) -> Option<Object> {
    match literal {
        ast::Literal::Int(i) => Some(Object::Int(i.clone())),
        ast::Literal::Bool(b) => Some(b.clone().into()),
        _ => Some(Object::Null),
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

    #[test]
    fn eval_prefix_expr_should_work() {
        let tests = vec![
            ("!true", Some(Object::Bool(false))),
            ("!false", Some(Object::Bool(true))),
            ("!!false", Some(Object::Bool(false))),
            ("!!true", Some(Object::Bool(true))),
            ("-1", Some(Object::Int(-1))),
            ("!-1", Some(Object::Bool(false))),
            ("!!-1", Some(Object::Bool(true))),
        ];

        tests.iter().for_each(|test| {
            let lexer = lexer::Lexer::new(test.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let obj = eval(&program);
            assert_eq!(
                test.1,
                obj,
                "want expr {} eval to be {:?}, got {:?}",
                test.0,
                test.1,
                eval(&program)
            );
        })
    }
}
