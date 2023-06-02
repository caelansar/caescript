use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::ast;

use super::env::Environment;
use crate::arithmetic_operator;

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
    Break,
    Continue,
    Function(
        Vec<ast::Ident>,
        ast::BlockStatement,
        Rc<RefCell<Environment>>,
    ),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Builtin(fn(Vec<Object>) -> Object),
    Null,
}

impl Eq for Object {}

impl std::hash::Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // we only support Int/String/Bool type as Hash key
        match self {
            Object::Int(i) => i.hash(state),
            Object::String(s) => s.0.hash(state),
            Object::Bool(b) => b.hash(state),
            _ => panic!("unsupport type"),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(i) => write!(f, "{}", i),
            Object::Float(float) => write!(f, "{}", float),
            Object::Bool(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "{}", s),
            Object::Return(r) => write!(f, "{}", r.to_string()),
            Object::Array(e) => write!(
                f,
                "[{}]",
                e.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            Object::Builtin(_) => write!(f, "[builtin]"),
            Object::Null => write!(f, "null"),
            _ => unreachable!(),
        }
    }
}

impl std::ops::Add for Object {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, +, Int, Float, String)
    }
}

impl std::ops::Sub for Object {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, -, Int, Float)
    }
}

impl std::ops::Mul for Object {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, *, Int, Float)
    }
}

impl std::ops::Div for Object {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        arithmetic_operator!(self, rhs, /, Int, Float)
    }
}

#[test]
fn object_display_should_work() {
    assert_eq!("123", Object::Int(123).to_string());
    assert_eq!("true", Object::Bool(true).to_string());
    assert_eq!("false", Object::Bool(false).to_string());
    assert_eq!("null", Object::Null.to_string());
}
