use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::ast;

use super::env::Environment;
use crate::obj_operator;

pub const BOOL_OBJ_TRUE: Object = Object::Bool(true);
pub const BOOL_OBJ_FALSE: Object = Object::Bool(false);

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub struct CString(pub String);

impl Display for CString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::ops::Add for CString {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        CString(format!("{}{}", self, rhs))
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        match value {
            true => BOOL_OBJ_TRUE,
            false => BOOL_OBJ_FALSE,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(CString),
    Return(Box<Object>),
    Function(
        Vec<ast::Ident>,
        ast::BlockStatement,
        Rc<RefCell<Environment>>,
    ),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(i) => write!(f, "{}", i),
            Object::Float(float) => write!(f, "{}", float),
            Object::Bool(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "{}", s),
            Object::Return(r) => write!(f, "{}", r.to_string()),
            Object::Null => write!(f, "null"),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Add for Object {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        obj_operator!(self, rhs, +, Int, Float, String)
    }
}

impl std::ops::Sub for Object {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        obj_operator!(self, rhs, -, Int, Float)
    }
}

impl std::ops::Mul for Object {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        obj_operator!(self, rhs, *, Int, Float)
    }
}

impl std::ops::Div for Object {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        obj_operator!(self, rhs, /, Int, Float)
    }
}

#[test]
fn object_display_should_work() {
    assert_eq!("123", Object::Int(123).to_string());
    assert_eq!("true", Object::Bool(true).to_string());
    assert_eq!("false", Object::Bool(false).to_string());
    assert_eq!("null", Object::Null.to_string());
}
