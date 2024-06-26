use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use super::{ast, builtin, env::Environment};

pub const BOOL_OBJ_TRUE: Object = Object::Bool(true);
pub const BOOL_OBJ_FALSE: Object = Object::Bool(false);

#[derive(Debug, PartialEq, PartialOrd, Clone, Default)]
pub struct Instructions(pub Vec<u8>);

#[derive(PartialEq, Clone, PartialOrd)]
pub struct CString(pub Rc<str>);

impl Display for CString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl Debug for CString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl From<&str> for CString {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

impl std::ops::Add for CString {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        CString(format!("{}{}", self, rhs).into())
    }
}

impl std::ops::Add for &CString {
    type Output = CString;

    fn add(self, rhs: Self) -> Self::Output {
        CString(format!("{}{}", self, rhs).into())
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
    Builtin(builtin::Builtin),
    Error(String),
    CompiledFunction(Rc<Instructions>, usize, usize), // (fn, num_locals, num_params)
    Closure(Closure),
    Null,
}

// A closure is an expression with free variables. The real role
// of free variables depends on its referencing lexical environment
// A free variables is not a local variable or a parameter and its
// scope is within the enclosing function
#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub func: CompiledFunction,
    // Make sure all identical closure refer to same free variables.
    // If we just clone `Closure`, the changes in free variables
    // cannot be reflected in the next call
    pub free: Rc<RefCell<Vec<Object>>>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct CompiledFunction {
    pub(crate) instructions: Rc<Instructions>,
    pub(crate) num_locals: usize,
    pub(crate) num_params: usize,
}

impl From<&Object> for bool {
    fn from(value: &Object) -> Self {
        match value {
            Object::Bool(b) => *b,
            Object::Null => false,
            _ => true,
        }
    }
}

impl Eq for Object {}

impl std::hash::Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // we only support Int/String/Bool type as Hash key
        match self {
            Object::Int(i) => i.hash(state),
            Object::String(s) => s.0.hash(state),
            Object::Bool(b) => b.hash(state),
            _ => panic!("unsupported type"),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(i) => write!(f, "{}", i),
            Object::Float(float) => write!(f, "{}", float),
            Object::Bool(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "\"{}\"", s),
            Object::Return(r) => write!(f, "{}", r),
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
            Object::Error(e) => write!(f, "err: {}", e),
            Object::Break => write!(f, "break"),
            Object::Continue => write!(f, "continue"),
            Object::Hash(ref hash) => {
                let mut result = String::new();
                for (i, (k, v)) in hash.iter().enumerate() {
                    if i < 1 {
                        result.push_str(&format!("{}: {}", k, v));
                    } else {
                        result.push_str(&format!(", {}: {}", k, v));
                    }
                }
                write!(f, "{{{}}}", result)
            }
            Object::Function(ref params, _, _) => {
                let mut result = String::new();
                for (i, ast::Ident(ref s)) in params.iter().enumerate() {
                    if i < 1 {
                        result.push_str(s);
                    } else {
                        result.push_str(&format!(", {}", s));
                    }
                }
                write!(f, "fn({}) {{  }}", result)
            }
            Object::CompiledFunction(_, _, _) => {
                write!(f, "compiled_fn()")
            }
            Object::Closure(_) => {
                write!(f, "closure()")
            }
        }
    }
}

#[test]
fn cstring_should_work() {
    let s1 = CString("aa".into());
    let s2 = CString("bb".into());

    assert_eq!(CString("aabb".into()), &s1 + &s2);
    assert_eq!(CString("aabb".into()), s1 + s2);
}

#[test]
fn object_bool_should_work() {
    let b1 = &Object::Bool(true);
    let b2 = &Object::Bool(false);

    assert_eq!(true, b1.into());
    assert_eq!(false, b2.into());
}

#[test]
fn object_display_should_work() {
    assert_eq!("123", Object::Int(123).to_string());
    assert_eq!("true", Object::Bool(true).to_string());
    assert_eq!("false", Object::Bool(false).to_string());
    assert_eq!("null", Object::Null.to_string());
    assert_eq!("\"aa\"", Object::String("aa".into()).to_string());
}

#[test]
fn object_arithmetic_should_work() {
    assert_eq!(
        Object::Error("type mismatch: 123 + 1.1".into()),
        &Object::Int(123) + &Object::Float(1.1)
    );
    assert_eq!(
        Object::Error("type mismatch: 123 + \"4\"".into()),
        &Object::Int(123) + &Object::String("4".into())
    );
    assert_eq!(Object::Int(124), &Object::Int(123) + &Object::Int(1));
    assert_eq!(
        Object::String("12".into()),
        &Object::String("1".into()) + &Object::String("2".into())
    );
    assert_eq!(
        Object::Error("unsupported operator: [] + []".into()),
        &Object::Array(Vec::new()) + &Object::Array(Vec::new())
    );
    assert_eq!(Object::Int(1), &Object::Int(1) % &Object::Int(2));
}
