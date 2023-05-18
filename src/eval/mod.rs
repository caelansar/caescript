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
        ast::Statement::Return(ret) => eval_expression(&ret),
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
        ast::Expression::Infix(infix, lhs, rhs) => {
            let lhs = eval_expression(lhs);
            let rhs = eval_expression(rhs);
            if lhs.is_none() || rhs.is_none() {
                return None;
            }
            eval_infix_expression(infix, lhs.unwrap(), rhs.unwrap())
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

fn eval_infix_expression(infix: &ast::Infix, lhs: Object, rhs: Object) -> Option<Object> {
    match lhs {
        Object::Int(l) => {
            if let Object::Int(r) = rhs {
                match infix {
                    ast::Infix::Plus => Some(Object::Int(l + r)),
                    ast::Infix::Minus => Some(Object::Int(l - r)),
                    ast::Infix::Divide => Some(Object::Int(l / r)),
                    ast::Infix::Multiply => Some(Object::Int(l * r)),
                    ast::Infix::Eq => Some(Object::Bool(l == r)),
                    ast::Infix::Ne => Some(Object::Bool(l != r)),
                    ast::Infix::Gt => Some(Object::Bool(l > r)),
                    ast::Infix::GtEq => Some(Object::Bool(l >= r)),
                    ast::Infix::Lt => Some(Object::Bool(l < r)),
                    ast::Infix::LtEq => Some(Object::Bool(l <= r)),
                }
            } else {
                None
            }
        }
        Object::Bool(l) => {
            if let Object::Bool(r) = rhs {
                match infix {
                    ast::Infix::Eq => Some(Object::Bool(l == r)),
                    ast::Infix::Ne => Some(Object::Bool(l != r)),
                    _ => None,
                }
            } else {
                None
            }
        }
        Object::String(l) => {
            if let Object::String(r) = rhs {
                match infix {
                    ast::Infix::Plus => Some(Object::String(format!("{}{}", l, r))),
                    ast::Infix::Eq => Some(Object::Bool(l == r)),
                    ast::Infix::Ne => Some(Object::Bool(l != r)),
                    ast::Infix::Gt => Some(Object::Bool(l > r)),
                    ast::Infix::GtEq => Some(Object::Bool(l >= r)),
                    ast::Infix::Lt => Some(Object::Bool(l < r)),
                    ast::Infix::LtEq => Some(Object::Bool(l <= r)),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
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
        ast::Literal::String(s) => Some(Object::String(s.clone())),
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
    fn eval_expr_should_work() {
        let tests = vec![
            ("!true", Some(Object::Bool(false))),
            ("!false", Some(Object::Bool(true))),
            ("!!false", Some(Object::Bool(false))),
            ("!!true", Some(Object::Bool(true))),
            ("-1", Some(Object::Int(-1))),
            ("!-1", Some(Object::Bool(false))),
            ("!!-1", Some(Object::Bool(true))),
            ("true == true", Some(Object::Bool(true))),
            ("false == false", Some(Object::Bool(true))),
            ("1 == 1", Some(Object::Bool(true))),
            ("1 < 2", Some(Object::Bool(true))),
            ("1 > 0", Some(Object::Bool(true))),
            ("1 >= 1", Some(Object::Bool(true))),
            ("1 <= 1", Some(Object::Bool(true))),
            ("1 - 1", Some(Object::Int(0))),
            ("1 + 1", Some(Object::Int(2))),
            ("1 * 1", Some(Object::Int(1))),
            ("1 / 1", Some(Object::Int(1))),
            ("1 + 4 == 5", Some(Object::Bool(true))),
            (
                r#""hello "+"world""#,
                Some(Object::String("hello world".to_string())),
            ),
            (r#""hello" == "hello""#, Some(Object::Bool(true))),
            (r#""hello" != "hello""#, Some(Object::Bool(false))),
            (r#""a" < "b""#, Some(Object::Bool(true))),
            (r#""a" > "b""#, Some(Object::Bool(false))),
            (r#""a" >= "a""#, Some(Object::Bool(true))),
            (r#""a" <= "a""#, Some(Object::Bool(true))),
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

    #[test]
    fn eval_return_should_work() {
        let tests = vec![
            ("return true", Some(Object::Bool(true))),
            ("return false", Some(Object::Bool(false))),
            ("return 1+2", Some(Object::Int(3))),
            (r#"return "1""#, Some(Object::String("1".to_string()))),
        ];

        tests.iter().for_each(|test| {
            let lexer = lexer::Lexer::new(test.0);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let obj = eval(&program);
            assert_eq!(
                test.1,
                obj,
                "want return stmt {} eval to be {:?}, got {:?}",
                test.0,
                test.1,
                eval(&program)
            );
        })
    }
}
